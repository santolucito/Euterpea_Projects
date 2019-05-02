-----------------------------------------------------------------------------
-- |
-- Module : Currency
--
-- Applicative Interface for currency.
--
-----------------------------------------------------------------------------

{-# LANGUAGE Rank2Types, RecursiveDo #-}

-----------------------------------------------------------------------------

module Currency
  ( currency
  ) where

-----------------------------------------------------------------------------

import Control.Monad.Fix

-----------------------------------------------------------------------------

currency
  :: (MonadFix m, Applicative signal)
     -- cell implementation
  => (forall poly. poly -> signal poly -> m (signal poly))
     -- dollarToEuro
  -> (a -> d)
     -- euroToDollar
  -> (b -> c)
     -- initial value: dollarDisplay
  -> c
     -- initial value: euroDisplay
  -> d
     -- dollar (input)
  -> signal a
     -- euro (input)
  -> signal b
     -- outputs
  -> m ( -- dollarDisplay
         signal c
      , -- euroDisplay
       signal d
      )

currency
  cell
  f_dollarToEuro
  f_euroToDollar
  i_dollarDisplay
  i_euroDisplay
  s_dollar
  s_euro

  = do
      rec
        c_dollarDisplay <- cell i_dollarDisplay o_dollarDisplay
        c_euroDisplay <- cell i_euroDisplay o_euroDisplay

        let w4 = f_dollarToEuro <$> s_dollar
        let w5 = f_euroToDollar <$> s_euro

        (cout0, cout1, cout2, cout3) <-
          controlCircuit
            cell

        let o_dollarDisplay =
              dollarDisplaySwitch
                c_dollarDisplay
                cout0
                w5
                cout1
        let o_euroDisplay =
              euroDisplaySwitch
                c_euroDisplay
                cout2
                w4
                cout3
    
      return (o_dollarDisplay, o_euroDisplay)

-----------------------------------------------------------------------------

dollarDisplaySwitch
  :: Applicative signal
  => signal a
  -> signal Bool
  -> signal a
  -> signal Bool
  -> signal a

dollarDisplaySwitch s0 b0 s1 _ =
  let ite b s a = (\b s a -> if b then s else a) <$> b <*> s <*> a
  in ite b0 s0 s1

-----------------------------------------------------------------------------

euroDisplaySwitch
  :: Applicative signal
  => signal a
  -> signal Bool
  -> signal a
  -> signal Bool
  -> signal a

euroDisplaySwitch s0 b0 s1 _ =
  let ite b s a = (\b s a -> if b then s else a) <$> b <*> s <*> a
  in ite b0 s0 s1

-----------------------------------------------------------------------------

controlCircuit
  :: (MonadFix m, Applicative signal)
     -- cell implementation
  =>  (Bool -> signal Bool -> m (signal Bool))
     -- inputs
     -- outputs
  -> m ( signal Bool
      , signal Bool
      , signal Bool
      , signal Bool
      )

controlCircuit cell =
  let
  in
    return ((pure False), (pure True), (pure False), (pure True))

  where
    _lat_ = cell False
    _and_ x y = (&&) <$> x <*> y

-----------------------------------------------------------------------------
