{-# LANGUAGE MultiParamTypeClasses 	#-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
-- Basis: http://hackage.haskell.org/package/foreign-var-0.1/docs/Foreign-Var.html
module StateVar where
data StateVar a = StateVar (IO a) (a -> IO ())

type GettableStateVar = IO

newtype SettableStateVar a = SettableStateVar (a -> IO ())

infixr 2 $=

class HasSetter t a | t -> a where
	($=) :: t -> a -> IO ()

instance HasSetter (StateVar a) a where
	(StateVar _ setter) $= a = setter a

class HasGetter t a | t -> a where
	get :: t -> IO a

instance HasGetter (StateVar a) a where
	get (StateVar getter _) = getter

instance HasGetter (GettableStateVar a) a where
	get = id

instance HasSetter (SettableStateVar a) a where
	SettableStateVar setter $= a = setter a
