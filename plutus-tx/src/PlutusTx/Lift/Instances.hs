-- editorconfig-checker-disable-file
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module PlutusTx.Lift.Instances () where

import PlutusTx.Bool (Bool (..))
import PlutusTx.Either (Either (..))
import PlutusTx.Lift.TH
import PlutusTx.Maybe (Maybe (..))

-- Standard types
-- These need to be in a separate file for TH staging reasons

{-@ ignore liftBool @-}
{-@ ignore liftUnit @-}
{-@ ignore liftList @-}

liftBool = makeLift ''Bool
makeLift ''Maybe
makeLift ''Either
liftList = makeLift ''[]
liftUnit = makeLift ''()
-- include a few tuple instances for convenience
makeLift ''(,)
makeLift ''(,,)
makeLift ''(,,,)
makeLift ''(,,,,)

