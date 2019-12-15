{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Base where

import Data.Proxy (Proxy (Proxy))
import TypeDSL
import GHC.TypeLits
import qualified Data.Text as T

true :: Value 'VBool
true = ValueBool True

false :: Value 'VBool
false = ValueBool False

param :: forall s a f m. (Applicative m, Param f s a) => f -> m (Value a)
param = pure . param' (Proxy :: Proxy s) (Proxy :: Proxy a)

define :: forall s m a. (Applicative m, KnownSymbol s) => Value a -> m (Value a)
define _ = pure $ ValueDef @a (T.pack (symbolVal (Proxy @s)))

println :: Applicative m => String -> m ()
println _ = pure ()

pass :: forall s a. Value a -> (s >> a)
pass _ = Binding

canCall :: forall name l l' ret. (Matches (Fn name l ret) l')
        => Fn name l ret -> ParamList l' -> Bool
canCall f0 _ = matches @_ @l' f0

canCall' :: forall name l l' m ret. (Matches (Fn name l ret) l')
         => FnM name l m ret -> ParamList l' -> Bool
canCall' (f :=: _) _ = matches @_ @l' f

call :: forall name l l' m ret. Monad m => (Matches (Fn name l ret) l')
     => FnM name l m ret -> ParamList l' -> m (Value ret)
call a b
  | canCall' a b = return $ Value @ret
  | otherwise    = error "Parameter list does not match"
