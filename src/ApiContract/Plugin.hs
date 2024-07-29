
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}


module ApiContract.Plugin where

#if __GLASGOW_HASKELL__ >= 900
import Language.Haskell.Syntax.Type
import GHC.Hs.Extension ()
import GHC.Parser.Annotation ()
import GHC.Utils.Outputable (docToSDoc)
import qualified Data.IntMap.Internal as IntMap
import GHC.Data.Bag
import GHC.HsToCore
import GHC.Types.SrcLoc
import GHC.Driver.Errors
import GHC.Unit.Types
import GHC.Driver.Backpack.Syntax
import GHC.Unit.Info
-- import Streamly.Internal.Data.Stream (fromList,mapM_,mapM,toList)
import GHC hiding (typeKind)
import GHC.Driver.Plugins (Plugin(..),CommandLineOption,defaultPlugin,PluginRecompile(..))
import GHC.Driver.Env
import GHC.Tc.Types
import GHC.Unit.Module.ModSummary
import GHC.Utils.Outputable (showSDocUnsafe,ppr,SDoc,Outputable,reallyAlwaysQualify)
import GHC.Data.Bag (bagToList)
import GHC.Types.Name hiding (varName)
import GHC.Types.Var
import qualified Data.Aeson.KeyMap as HM
import qualified Data.Aeson.Key as HM
import GHC.Core.Opt.Monad
import GHC.Rename.HsType
import qualified GHC.Tc.Utils.Monad as TCError
import qualified GHC.Types.SourceError as ParseError
import qualified GHC.Types.Error as ParseError
import GHC.Types.Name.Reader ( rdrNameOcc ,rdrNameSpace)
import GHC.Core.TyCo.Rep
import GHC.Data.FastString
import GHC.IO (unsafePerformIO)
import qualified GHC.Utils.Ppr as Pretty
#else
import FastString
import CoreMonad (CoreM, CoreToDo (CoreDoPluginPass), liftIO)
import CoreSyn (
    AltCon (..),
    Bind (NonRec, Rec),
    CoreBind,
    CoreExpr,
    Expr (..),
    mkStringLit
 )
import TyCoRep
import GHC.IO (unsafePerformIO)
import GHC.Hs
import GHC.Hs.Decls
import GhcPlugins (
    CommandLineOption,Arg (..),
    HsParsedModule(..),
    Hsc,
    Name,SDoc,DataCon,DynFlags,ModSummary(..),TyCon,
    Literal (..),typeEnvElts,
    ModGuts (mg_binds, mg_loc, mg_module),showSDoc,
    Module (moduleName),tyConKind,
    NamedThing (getName),getDynFlags,tyConDataCons,dataConOrigArgTys,dataConName,
    Outputable (..),dataConFieldLabels,PluginRecompile(..),
    Plugin (..),
    Var,flLabel,dataConRepType,
    coVarDetails,
    defaultPlugin,
    idName,
    mkInternalName,
    mkLitString,
    mkLocalVar,
    mkVarOcc,
    moduleNameString,
    nameStableString,
    noCafIdInfo,
    purePlugin,
    showSDocUnsafe,
    tyVarKind,
    unpackFS,
    tyConName,
    msHsFilePath
 )
import Id (isExportedId,idType)
import Name (getSrcSpan)
import SrcLoc
import Unique (mkUnique)
import Var (isLocalId,varType)
import TcRnTypes
import TcRnMonad
import DataCon
#endif

import Control.Reference (biplateRef, (^?))
import ApiContract.Types
-- import Data.Aeson
import Data.List.Extra (intercalate, isSuffixOf, replace, splitOn,groupBy)
import Data.List ( sortBy, intercalate ,foldl')
import qualified Data.Map as Map
import Data.Text (Text, concat, isInfixOf, pack, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time
import Data.Map (Map)
import Data.Data
import Data.Maybe (catMaybes,isJust,fromJust)
import Control.Monad.IO.Class (liftIO)
import System.IO (writeFile)
import Streamly.Internal.Data.Stream hiding (concatMap, init, length, map, splitOn,foldl',intercalate)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Directory.Internal.Prelude hiding (mapM, mapM_)
import Prelude hiding (id, mapM, mapM_)
import Control.Exception (evaluate)
import qualified Data.Record.Plugin as DRP
import qualified Data.Record.Anon.Plugin as DRAP
import qualified Data.Record.Plugin.HasFieldPattern as DRPH
import qualified RecordDotPreprocessor as RDP
import qualified Data.Yaml as YAML
import Control.Monad (foldM)
import Data.Char
import qualified Data.ByteString as DBS
import Data.Bool (bool)
import GHC (noLoc)
import Data.Aeson.Encode.Pretty (mempty)
import GHC.ExecutionStack (Location(srcLoc))
import Streamly.Internal.Data.Parser (ParseError(ParseError))
import ApiContract.Types
import Data.List (nub)

plugin :: Plugin
plugin = (defaultPlugin{
            -- installCoreToDos = install
        pluginRecompile = (\_ -> return NoForceRecompile)
        , parsedResultAction = collectTypeInfoParser
        , typeCheckResultAction = collectInstanceInfo
        })
#if defined(ENABLE_LR_PLUGINS)
        <> DRP.plugin
        <> DRAP.plugin
        <> DRPH.plugin
#endif
        <> RDP.plugin

instance Semigroup Plugin where
  p <> q = defaultPlugin {
      parsedResultAction = \args summary ->
            parsedResultAction p args summary
        >=> parsedResultAction q args summary
    ,  typeCheckResultAction = \args summary ->
            typeCheckResultAction p args summary
        >=> typeCheckResultAction q args summary
    , pluginRecompile = \args ->
            (<>)
        <$> pluginRecompile p args
        <*> pluginRecompile q args
    , tcPlugin = \args -> 
        case (tcPlugin p args, tcPlugin q args) of
          (Nothing, Nothing) -> Nothing
          (Just tp, Nothing) -> Just tp
          (Nothing, Just tq) -> Just tq
          (Just (TcPlugin tcPluginInit1 tcPluginSolve1 tcPluginStop1), Just (TcPlugin tcPluginInit2 tcPluginSolve2 tcPluginStop2)) -> Just $ TcPlugin 
            { tcPluginInit = do
                ip <- tcPluginInit1
                iq <- tcPluginInit2
                return (ip, iq)
            , tcPluginSolve = \(sp,sq) given derived wanted -> do
                solveP <- tcPluginSolve1 sp given derived wanted
                solveQ <- tcPluginSolve2 sq given derived wanted
                return $ combineTcPluginResults solveP solveQ
            , tcPluginStop = \(solveP,solveQ) -> do
                tcPluginStop1 solveP
                tcPluginStop2 solveQ
            }
    }

combineTcPluginResults :: TcPluginResult -> TcPluginResult -> TcPluginResult
combineTcPluginResults resP resQ =
  case (resP, resQ) of
    (TcPluginContradiction ctsP, TcPluginContradiction ctsQ) ->
      TcPluginContradiction (ctsP ++ ctsQ)
 
    (TcPluginContradiction ctsP, TcPluginOk _ _) ->
      TcPluginContradiction ctsP
 
    (TcPluginOk _ _, TcPluginContradiction ctsQ) ->
      TcPluginContradiction ctsQ
 
    (TcPluginOk solvedP newP, TcPluginOk solvedQ newQ) ->
      TcPluginOk (solvedP ++ solvedQ) (newP ++ newQ)
 

instance Monoid Plugin where
  mempty = defaultPlugin

pprTyCon :: Name -> SDoc
pprTyCon = ppr

pprDataCon :: Name -> SDoc
pprDataCon = ppr

collectTypeInfoParser :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
collectTypeInfoParser opts modSummary hpm = do
    let prefixPath = "./.juspay/api-contract/"
        moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
        modulePath = prefixPath <> msHsFilePath modSummary
        moduleSrcSpan = mkFileSrcSpan $ ms_location modSummary
        hm_module = unLoc $ hpm_module hpm
        path = (intercalate "/" . init . splitOn "/") modulePath
    liftIO $ createDirectoryIfMissing True path
    typesInThisModule <- liftIO $ toList $ mapM (pure . getTypeInfo) (fromList $ hsmodDecls hm_module)
    let (srcSpansHM :: HM.KeyMap SrcSpan) = HM.fromList $ map (\(srcSpan,a,_) -> (HM.fromString a, srcSpan)) $ Prelude.concat typesInThisModule
        (typeVsFields :: HM.KeyMap TypeRule) = HM.fromList $ map (\(_,a,b) -> (HM.fromString a, b)) $ Prelude.concat typesInThisModule
    if generateTypesRules
        then
            liftIO $ DBS.writeFile (modulePath <> ".yaml") (YAML.encode typeVsFields)
        else do
            (typeRules :: HM.KeyMap TypeRule) <- liftIO $ fetchRules (modulePath <> ".yaml")
            errors :: [[(SrcSpan,ApiContractError)]] <- liftIO $ toList $ mapM (\(typeName,rules) -> do
                                    case HM.lookup typeName typeVsFields of
                                        Just typeRule -> do
                                            let srcSpan = fromJust $ HM.lookup typeName srcSpansHM
                                            errorList <- runFieldNameAndTypeRule (HM.toString typeName) (caseType rules) (dataConstructors rules) (dataConstructors typeRule)
                                            pure $ map (\x -> (srcSpan,x)) $ errorList
                                        Nothing -> pure [(moduleSrcSpan,(MISSNNG_TYPE (HM.toString typeName)))]
                            ) (fromList $ HM.toList typeRules)
            errorsNubbed :: [(SrcSpan,ApiContractError)] <- pure $ Data.List.nub $ Prelude.concat errors
            if (not $ Prelude.null $ errorsNubbed)
                then do
                    errorMessages <- pure $ listToBag $ map (\(srcSpan,errorMessage) -> ParseError.mkErr srcSpan reallyAlwaysQualify (ParseError.mkDecorated [docToSDoc $ Pretty.text $ show errorMessage])) errorsNubbed
                    ParseError.throwErrors errorMessages
                else pure ()
    pure hpm

collectInstanceInfo :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
collectInstanceInfo opts modSummary tcEnv = do
    let prefixPath = "./.juspay/api-contract/"
        moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
        modulePath = prefixPath <> msHsFilePath modSummary
        moduleSrcSpan = mkFileSrcSpan $ ms_location modSummary
        path = (intercalate "/" . init . splitOn "/") modulePath
    typeInstances <- liftIO $ toList $ mapM processInstance (fromList $ bagToList $ tcg_binds tcEnv)
    (instanceSrcSpansHM :: HM.KeyMap SrcSpan) <- pure $ HM.fromList $ map (\(srcSpan,typeName,instanceName,_) -> (HM.fromString (typeName <> "--" <> instanceName), srcSpan)) $ Prelude.concat typeInstances
    (instanceTypeHM :: HM.KeyMap InstanceFromTC) <- pure $ HM.fromList $ map (\(_,typeName,instanceName,x) -> (HM.fromString (typeName <> "--" <> instanceName), x)) $ Prelude.concat typeInstances
    (typeRules :: HM.KeyMap TypeRule) <- liftIO $ fetchRules (modulePath <> ".yaml")
    if generateTypesRules
        then do
            liftIO $ print "dumping rules"
            let updatedTypesRules = foldl' (\hm (_,typeName,instanceName,x) ->
                                            case HM.lookup (HM.fromString typeName) hm of
                                                Just v  -> HM.insert (HM.fromString typeName) (v{instances = Map.insert instanceName x (instances v)}) hm
                                                Nothing -> hm
                                        ) typeRules $ Prelude.concat typeInstances
            liftIO $ DBS.writeFile (modulePath <> ".yaml") (YAML.encode updatedTypesRules)
        else do
            errors :: [[(SrcSpan,ApiContractError)]] <- liftIO $ toList $ mapM (\(typeName,rules) ->
                                    let (instancesMap :: Map.Map String InstanceFromTC) = instances rules
                                        errors = map (\(x,ruleInst) ->
                                                    case HM.lookup (typeName <> (HM.fromString "--") <> (HM.fromString x)) instanceTypeHM of
                                                        Just inst ->
                                                            let instanceSpan = fromJust $ HM.lookup (typeName <> (HM.fromString "--") <> (HM.fromString x)) instanceSrcSpansHM
                                                                typeOfInstanceCheck =
                                                                            if (typeOfInstance inst) == (typeOfInstance ruleInst)
                                                                                then mempty
                                                                                else [(instanceSpan,(TYPE_OF_INSTANCE_CHANGED (HM.toString typeName) x (typeOfInstance ruleInst)))]
                                                                fieldListCheck =
                                                                        map (\y ->
                                                                                    if y `Prelude.elem` fieldsList inst
                                                                                        then mempty
                                                                                        else [(instanceSpan,(MISSING_FIELD_IN_INSTANCE (HM.toString typeName) x y))]
                                                                            ) (fieldsList ruleInst)
                                                            in typeOfInstanceCheck <> (Prelude.concat $ fieldListCheck)
                                                        Nothing -> [(moduleSrcSpan, (MISSING_INSTANCE (HM.toString typeName) x))]
                                                ) $ Map.toList instancesMap
                                    in pure $ Prelude.concat errors
                            ) (fromList $ HM.toList typeRules)
            errorsNubbed :: [(SrcSpan,ApiContractError)] <- pure $ Data.List.nub $ Prelude.concat errors
            if (not $ Prelude.null $ errorsNubbed)
                then do
                    TCError.addErrs $ map (\(srcSpan,errorMessage) -> (srcSpan,docToSDoc $ Pretty.text $ show errorMessage)) errorsNubbed
                else pure ()
    pure tcEnv

processInstance :: LHsBindLR GhcTc GhcTc -> IO [(SrcSpan,String,String,InstanceFromTC)]
processInstance (L l (FunBind _ id' matches _)) = do
  let instanceFunctionName = replace "$_in$" "" $ nameStableString $ getName id'
      stmts = (mg_alts matches) ^? biplateRef :: [LHsExpr GhcTc]
      possibleFields = map (\(_,val) -> getLit $ unXRec @(GhcTc) val) $ Prelude.filter (\(constr,_) -> constr `Prelude.elem` ["HsLit"] ) $ map ((\x -> (show $ toConstr $ unLoc x,x))) (stmts)
      typeSignature = getAppliedOnTypeName instanceFunctionName $ varType (unLoc id')
  if isJust typeSignature then
    if Prelude.null possibleFields
      then pure [(locA l,fromMaybe mempty typeSignature,instanceFunctionName,InstanceFromTC possibleFields Derived)]
      else pure [(locA l,fromMaybe mempty typeSignature,instanceFunctionName,InstanceFromTC possibleFields Custom)]
    else pure mempty
processInstance (L _ (VarBind{var_id = var, var_rhs = expr})) = pure mempty
processInstance (L _ (PatBind{pat_lhs = pat, pat_rhs = expr})) = pure mempty
processInstance (L _ (AbsBinds{abs_binds = binds})) = do
  res <- toList $ mapM processInstance $ fromList $ bagToList binds
  pure $ Prelude.concat res
processInstance _ = pure mempty

getLit :: HsExpr p -> [Char]
getLit (HsLit _ (HsChar _  char)) = [char]
getLit (HsLit _ (HsCharPrim _  char)) = [char]
getLit (HsLit _ (HsString _  fs)) = unpackFS  fs
getLit (HsLit _ (HsStringPrim _  bs)) = unpack $ decodeUtf8 bs
getLit _ = mempty

getAppliedOnTypeName :: String -> Type -> Maybe String
getAppliedOnTypeName "parseJSON" (FunTy _ _ arg (TyConApp _ types)) =  Just $ (showSDocUnsafe $ ppr $ Prelude.last types)
getAppliedOnTypeName "toJSON" (FunTy _ _ arg res) = Just $ (showSDocUnsafe $ ppr arg)
getAppliedOnTypeName "toEncoding" (FunTy _ _ arg res) = Just $ (showSDocUnsafe $ ppr arg)
getAppliedOnTypeName _ _ = Nothing

getTypeInfo :: LHsDecl GhcPs -> [(SrcSpan,String,TypeRule)]
getTypeInfo (L l (TyClD _ (DataDecl _ lname _ _ defn))) =
  [(locA l ,showSDocUnsafe' lname ,TypeRule
    { typeKind = "data"
    , caseType = Nothing
    , instances = mempty
    , dataConstructors = Map.fromList $ map getDataConInfo (dd_cons defn)
    })]
getTypeInfo (L l (TyClD _ (SynDecl _ lname _ _ rhs))) =
    [(locA l ,showSDocUnsafe' lname,TypeRule
    { typeKind = "type"
    , caseType = Nothing
    , instances = mempty
#if __GLASGOW_HASKELL__ >= 900
    , dataConstructors = Map.singleton (showSDocUnsafe' lname) (DataConInfo  (maybe mempty (Map.singleton "synonym" . unpackHDS) (hsTypeToString $ unLoc rhs)) [])
#else
    , dataConstructors = Map.singleton (showSDocUnsafe' lname) (DataConInfo (Map.singleton "synonym" ((showSDocUnsafe . ppr . unLoc) rhs)) [])
#endif
    })]
getTypeInfo _ = mempty

instance Outputable Void where

getDataConInfo :: LConDecl GhcPs -> (String,DataConInfo)
getDataConInfo (L _ ConDeclH98{ con_name = lname, con_args = args }) =
    (showSDocUnsafe' lname,DataConInfo
      { fields' = getFieldMap args
      , sumTypes = [] -- For H98-style data constructors, sum types are not applicable
      })
getDataConInfo (L _ ConDeclGADT{ con_names = lnames, con_res_ty = ty }) =
  (intercalate ", " (map showSDocUnsafe' lnames),DataConInfo
    {
#if __GLASGOW_HASKELL__ >= 900
    fields' = maybe (mempty) (\x -> Map.singleton "gadt" $ unpackHDS x) (hsTypeToString $ unLoc ty)
#else
    fields' = Map.singleton "gadt" (showSDocUnsafe $ ppr ty)
#endif
    , sumTypes = [] -- For GADT-style data constructors, sum types can be represented by the type itself
    })

#if __GLASGOW_HASKELL__ >= 900
hsTypeToString :: HsType GhcPs -> Maybe HsDocString
hsTypeToString = f
  where
    f :: HsType GhcPs -> Maybe HsDocString
    f (HsDocTy _ _ lds) = Just (unLoc lds)
    f (HsBangTy _ _ (L _ (HsDocTy _ _ lds))) = Just (unLoc lds)
    f x = Just (mkHsDocString $ showSDocUnsafe $ ppr x)

extractInfixCon :: [HsType GhcPs] -> Map.Map String String
extractInfixCon x =
  let l = length x
  in Map.fromList $ map (\(a,b) -> (show a , b)) $ Prelude.zip [0..l] (map f x)
  where
    f :: HsType GhcPs -> (String)
    f (HsDocTy _ _ lds) = showSDocUnsafe $ ppr $ (unLoc lds)
    f (HsBangTy _ _ (L _ (HsDocTy _ _ lds))) = showSDocUnsafe $ ppr $ (unLoc lds)
    f x = (showSDocUnsafe $ ppr x)

extractConDeclField :: [ConDeclField GhcPs] -> Map.Map String String
extractConDeclField x = Map.fromList (go x)
  where
    go :: [ConDeclField GhcPs] -> [(String,String)]
    go [] = []
    go ((ConDeclField _ cd_fld_names cd_fld_type _):xs) =
        [((intercalate "," $ convertRdrNameToString cd_fld_names),(showSDocUnsafe $ ppr cd_fld_type))] <> (go xs)

    convertRdrNameToString x = map (showSDocUnsafe . ppr . rdrNameOcc . unLoc . reLocN . rdrNameFieldOcc . unXRec @(GhcPs)) x

getFieldMap :: HsConDeclH98Details GhcPs -> Map.Map String String
getFieldMap con_args =
  case con_args of
    PrefixCon _ args         -> extractInfixCon $ map (unLoc . hsScaledThing) args
    InfixCon arg1 arg2       -> extractInfixCon $ map (unLoc . hsScaledThing) [arg1,arg2]
    RecCon (fields)          -> extractConDeclField $ map unLoc $ (unXRec @(GhcPs)) fields

#else
getFieldMap :: HsConDeclDetails GhcPs -> Map String String
getFieldMap (PrefixCon args) = Map.fromList $ Prelude.zipWith (\i t -> (show i, showSDocUnsafe (ppr t))) [1..] args
getFieldMap (RecCon (L _ fields)) = Map.fromList $ concatMap getRecField fields
  where
    getRecField (L _ (ConDeclField _ fnames t _)) = [(showSDocUnsafe (ppr fname), showSDocUnsafe (ppr t)) | L _ fname <- fnames]
getFieldMap (InfixCon t1 t2) = Map.fromList [("field1", showSDocUnsafe (ppr t1)), ("field2", showSDocUnsafe (ppr t2))]
#endif

#if __GLASGOW_HASKELL__ >= 900
showSDocUnsafe' = showSDocUnsafe . ppr . GHC.unXRec @(GhcPs)
#else
showSDocUnsafe' = showSDocUnsafe . ppr
#endif

fetchRules :: String -> IO (HM.KeyMap TypeRule)
fetchRules = parseYamlFile

parseYamlFile :: String -> IO (HM.KeyMap TypeRule)
parseYamlFile path = do
    res <- YAML.decodeFileEither path
    case res of
        Left (err :: YAML.ParseException) -> do
            print err
            pure mempty
        Right val -> pure val

runFieldNameAndTypeRule :: String -> Maybe CaseType -> Map.Map String DataConInfo -> Map.Map String DataConInfo -> IO [ApiContractError]
runFieldNameAndTypeRule typeName caseType rules codeExtract = do
    foldM (\acc (dataConName,x) ->
                                case Map.lookup dataConName codeExtract of
                                    Just val -> do
                                        res <- toList $ mapM (checkCaseType caseType) (fromList $ Map.keys $ fields' val)
                                        res' <- checkIfAllFieldsArePresent (Map.keys $ fields' x) (Map.keys $ fields' val)
                                        res'' <- checkAllFieldTypes (HM.fromList $ map (\(a,b) -> (HM.fromString a, b)) $ Map.toList $ fields' x) (HM.fromList $ map (\(a,b) -> (HM.fromString a, b)) $ Map.toList $ fields' val)
                                        pure $ acc <> (Prelude.concat res) <> res' <> res''
                                    Nothing -> pure $ acc <> [(MISSING_DATACON  dataConName  typeName)]
                        ) mempty $ Map.toList rules
    where
        checkIfAllFieldsArePresent :: [String] -> [String] -> IO [ApiContractError]
        checkIfAllFieldsArePresent fromRules fromCode =
            foldM (\acc x ->
                    if x `Prelude.elem` fromCode then pure acc else pure $ acc <> [(MISSING_FIELD x typeName)]
                ) mempty (fromRules)

        checkAllFieldTypes :: HM.KeyMap String -> HM.KeyMap String -> IO [ApiContractError]
        checkAllFieldTypes fromRules fromCode =
            foldM (\acc (_fieldName,_type) ->
                        case HM.lookup _fieldName fromCode of
                            Just val -> if val == _type
                                            then pure acc
                                            else pure $ acc <> [(TYPE_MISMATCH (HM.toString _fieldName) (_type) (val) typeName)]
                            Nothing -> pure $ acc <> [(MISSING_FIELD (HM.toString _fieldName) typeName)]
                    ) mempty (HM.toList fromRules)

        isSnakeCase :: String -> Bool
        isSnakeCase = Prelude.all (\c -> isLower c || c == '_' || isAlphaNum c)

        isCamelCase :: String -> Bool
        isCamelCase s = Prelude.all (\(c, i) -> if i == 0 then isLower c else isAlphaNum c) (zip s [0..])

        isPascalCase :: String -> Bool
        isPascalCase s = Prelude.all (\(c, i) -> if i == 0 then isUpper c else isAlphaNum c) (zip s [0..])

        isKebabCase :: String -> Bool
        isKebabCase = Prelude.all (\c -> isLower c || c == '-' || isAlphaNum c)

        checkCaseType :: Maybe CaseType -> String -> IO [ApiContractError]
        checkCaseType (Just SnakeCase)  field =  bool (pure [(FIELD_CASE_MISMATCH typeName field SnakeCase)]) (pure mempty) $ isSnakeCase field
        checkCaseType (Just CamelCase)  field =  bool (pure [(FIELD_CASE_MISMATCH typeName field CamelCase)]) (pure mempty) $ isCamelCase field
        checkCaseType (Just PascalCase) field  = bool (pure [(FIELD_CASE_MISMATCH typeName field PascalCase)]) (pure mempty) $  isPascalCase field
        checkCaseType (Just KebabCase)  field =  bool (pure [(FIELD_CASE_MISMATCH typeName field KebabCase)]) (pure mempty) $ isKebabCase field
        checkCaseType _ _ = pure mempty

generateTypesRules :: Bool
generateTypesRules = readBool $ unsafePerformIO $ lookupEnv "DUMP_TYPE_RULES"
    where
        readBool :: Maybe String -> Bool
        readBool (Just "true") = True
        readBool (Just "True") = True
        readBool (Just "TRUE") = True
        readBool _ = False

mkFileSrcSpan :: ModLocation -> SrcSpan
mkFileSrcSpan mod_loc
  = case ml_hs_file mod_loc of
      Just file_path -> mkGeneralSrcSpan (mkFastString file_path)
      Nothing        -> interactiveSrcSpan