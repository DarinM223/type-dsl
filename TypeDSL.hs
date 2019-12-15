{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module TypeDSL where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString (fromString))
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Data.Text as T 

data VType = VInt | VText | VBool | VFloat deriving Show

data Value (a :: VType) where
  Value      :: Value a
  ValueDef   :: Text -> Value a
  ValueInt   :: Int -> Value 'VInt
  ValueText  :: Text -> Value 'VText
  ValueBool  :: Bool -> Value 'VBool

instance Show (Value a) where
  show Value         = "Value"
  show (ValueDef v)  = show v
  show (ValueInt v)  = show v
  show (ValueText v) = show v
  show (ValueBool v) = show v

instance IsString (Value 'VText) where
  fromString = ValueText . T.pack

instance Num (Value 'VInt) where
  (+) (ValueInt a) (ValueInt b) = ValueInt $ a + b
  (+) _ _                       = error "+ not defined for Value"

  (*) (ValueInt a) (ValueInt b) = ValueInt $ a * b
  (*) _ _                       = error "* not defined for Value"

  abs (ValueInt s) = ValueInt $ abs s
  abs _            = error "abs not defined for Value"

  signum (ValueInt s) = ValueInt $ signum s
  signum _            = error "signum not defined for Value"

  negate (ValueInt s) = ValueInt $ negate s
  negate _            = error "negate not defined for Value"

  fromInteger i = ValueInt (fromIntegral i :: Int)

data (s :: Symbol) >> (a :: VType) = Binding
                                   | BindingDefault (Value a)

data Fn (name :: Symbol) xs (ret :: VType) where
  FNil  :: Fn name '[] ret
  (:->) :: (s >> a) -> Fn name as ret -> Fn name (s >> a ': as) ret
infixr 4 :->

data FnM (name :: Symbol) xs m ret =
  Fn name xs ret :=: (Fn name xs ret -> m (Value ret))
infixr 3 :=:

class KnownSymbol s => Param f s a where
  param' :: Proxy s -> Proxy a -> f -> Value a

data ContainsResult = Contains | NotContains Symbol VType

type family (ContainsBinding s a l) :: ContainsResult where
  ContainsBinding s a (s >> a ': xs) = 'Contains
  ContainsBinding s a (_ ': xs)      = ContainsBinding s a xs
  ContainsBinding s a '[]            = 'NotContains s a

instance (KnownSymbol s, ContainsBinding s a l ~ 'Contains)
    => Param (Fn name l ret) s a where
  param' s _ _ = ValueDef $ T.pack $ symbolVal s

data ParamList l where
  PNil :: ParamList '[]
  (:>) :: (s >> a) -> ParamList l -> ParamList (s >> a ': l)

type family (HasBinding l b) :: Bool where
  HasBinding (b ': _) b    = 'True
  HasBinding (_ ': rest) b = HasBinding rest b
  HasBinding '[] b         = 'False

type family RemoveBinding l b where
  RemoveBinding (b ': rest) b  = rest
  RemoveBinding (b' ': rest) b = b' ': RemoveBinding rest b
  RemoveBinding '[] _          = '[]

class Matches f (l :: [Type]) where
  matches :: f -> Bool

class Matches' (flag :: Bool) f (l :: [Type]) where
  matches' :: f -> Bool

instance Matches (Fn name '[] ret) '[] where
  matches _ = True

instance ( HasBinding l b ~ flag
         , Matches' flag (Fn name (b ': bs) ret) l
         , Matches (Fn name bs ret) (RemoveBinding l b) )
    => Matches (Fn name (b ': bs) ret) l where
  matches f@(_ :-> rest) = matches' @flag @_ @l f
                        && matches @_ @(RemoveBinding l b) rest

instance Matches' 'True (Fn name (b ': bs) ret) l where
  matches' _ = True

instance Matches' 'False (Fn name (b ': bs) ret) l where
  matches' (Binding :-> _)          = False
  matches' (BindingDefault _ :-> _) = True

instance Show (Fn name '[] ret) where
  show _ = ""

instance (Show' (Fn name (s >> a ': xs) ret), KnownSymbol s, KnownSymbol name)
    => Show (Fn name (s >> a ': xs) ret) where
  show = show' True

class Show' a where
  show' :: Bool -> a -> String

instance Show' (Fn name '[] ret) where
  show' _ _ = ")"

instance (Show' (Fn name xs ret), KnownSymbol s, KnownSymbol name)
    => Show' (Fn name (s >> a ': xs) ret) where
  show' showName (binding :-> rest)
    | showName  = name ++ "(" ++ bindingStr
    | otherwise = ", " ++ bindingStr
   where
    key = symbolVal (Proxy :: Proxy s)
    name = symbolVal (Proxy :: Proxy name)
    bindingStr = case binding of
      Binding          -> key ++ show' False rest
      BindingDefault v -> key ++ " = " ++ show v ++ show' False rest

instance Show (Fn name xs ret) => Show (FnM name xs m ret) where
  show (f :=: _) = show f
