{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module MyDo(mdo) where
import Data.Semigroup

class VarArg a where
  mdo :: a

instance {-# OVERLAPS #-} Monoid m => VarArg m where
  mdo = mempty

instance VarArg (m -> m) where
  mdo m = m

instance (Semigroup m, VarArg (m -> r)) => VarArg (m -> m -> r) where
  mdo y x =  mdo (y <> x)
