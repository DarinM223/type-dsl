type-dsl
========

Uses type-level lists to check various types
when writing DSLs in Haskell that generate code in other languages.

Example function:

```haskell
func :: Monad m => FnM "func" '["foo" >> 'VInt, "bar" >> 'VText] m 'VInt
func = BindingDefault 2 :-> BindingDefault "bar" :-> FNil :=: \f -> do
  foo <- param @"foo" @'VInt f
  bar <- param @"bar" @'VText f
  a <- define @"a" (ValueInt 2)
  b <- define @"b" false
  call testFunction2 (pass @"bar" bar :> PNil)
```
