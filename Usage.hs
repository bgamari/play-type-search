module Usage where

import Data.Monoid
import Data.Generics
import Data.List (isSuffixOf)

import qualified GHC
import           GHC (GenLocated(L))
import qualified TypeRep
import           TypeRep (Type(..))
import qualified Unify
import qualified OccName
import qualified Var
import qualified Type
import VarEnv
import VarSet

foldBindsOfType :: (Monoid r)
                => GHC.Type -> (GHC.LHsBind GHC.Id -> r)
                -> GHC.LHsBinds GHC.Id -> r
foldBindsOfType ty f = everything mappend (mempty `mkQ` go)
  where
    go bind@(L _ (GHC.FunBind {GHC.fun_id=L _ fid}))
      | GHC.idType fid `Type.eqType` ty = f bind
    go _ = mempty

foldBindsContainingType :: Monoid r
                        => GHC.Type -> (GHC.LHsBind GHC.Id -> r)
                        -> GHC.LHsBinds GHC.Id -> r
foldBindsContainingType ty f = everything mappend (mempty `mkQ` go)
  where
    go bind@(L _ (GHC.FunBind {GHC.fun_id=L _ fid}))
      | getAny $ everything mappend (mempty `mkQ` containsType) (GHC.idType fid) = f bind
      where
        -- a type variable will unify with anything
        --containsType (TyVarTy _)       = mempty

        -- We first check whether the types match, allowing all type variables to vary.
        -- This, however, is too lenient: the matcher is free to introduce equalities
        -- between our template variables. So, if this matches we then take the
        -- resulting substitution
        containsType ty'
          | Just subst <- Unify.tcMatchTy tyVars strippedTy ty'
          , bijectiveSubst subst
                      = --trace (showSDoc unsafeGlobalDynFlags $ ppr (strippedTy, ty', Type.tyVarsOfType ty', subst, tyVars)) $
                        Any True
          | otherwise = --trace (showSDoc unsafeGlobalDynFlags $ ppr
                        --       $ let subst = Unify.tcMatchTy tyVars strippedTy ty'
                        --         in (subst, Type.tyVarsOfType ty'))
                        mempty
          where
            bijectiveSubst :: Type.TvSubst -> Bool
            bijectiveSubst (Type.TvSubst _ subst) = iter emptyVarSet (varEnvElts subst)
              where
                iter :: VarSet -> [Type] -> Bool
                iter _             []                = True
                iter claimedTyVars (TypeRep.TyVarTy tyVar:rest)
                  | tyVar `elemVarSet` claimedTyVars = False
                  | otherwise                        = iter (extendVarSet claimedTyVars tyVar) rest
                iter claimedTyVars (_:rest)          = iter claimedTyVars rest
    go _ = mempty

    -- We don't necessarily want to match on the foralls the user needed to
    -- merely bring type variables into scope
    stripForAlls :: VarSet -> Type -> (Type, VarSet)
    stripForAlls vars (ForAllTy var ty) = stripForAlls (VarSet.extendVarSet vars var) ty
    stripForAlls vars ty                = (ty, vars)
    (strippedTy, tyVars) = stripForAlls VarSet.emptyVarSet ty

    isPrimed var = "'" `isSuffixOf` OccName.occNameString (OccName.occName $ Var.varName var)
    templVars = VarSet.filterVarSet (not . isPrimed) tyVars

foldBindsContainingTyCon :: Monoid r
                        => GHC.TyCon -> (GHC.LHsBind GHC.Id -> r)
                        -> GHC.LHsBinds GHC.Id -> r
foldBindsContainingTyCon tyCon f = everything mappend (mempty `mkQ` go)
  where
    go bind@(L _ (GHC.FunBind {GHC.fun_id=L _ fid}))
      | getAny $ everything mappend (mempty `mkQ` containsTyCon) (GHC.idType fid) = f bind
      where
        containsTyCon tyCon' | tyCon == tyCon' = Any True
        containsTyCon _                        = mempty
    go _ = mempty

foldBindsContainingIdent :: Monoid r
                         => GHC.Id -> (GHC.LHsBind GHC.Id -> r)
                         -> GHC.LHsBinds GHC.Id -> r
foldBindsContainingIdent ident f = everything mappend (mempty `mkQ` go)
  where
    go bind
      | getAny $ everything mappend (mempty `mkQ` containsId) bind = f bind
      where
        containsId ident' | ident == ident' = Any True
        containsId _                        = mempty
    go _ = mempty
