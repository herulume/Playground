{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
module Lens where

import           Control.Lens
import           Control.Applicative
import           Data.Char
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import Data.Semigroup ((<>))

data Role = Gunner
          | PowderMonkey
          | Navigator
          | Captain
          | FirstMate deriving (Show, Eq, Ord)

data CrewMember =
    CrewMember { _name :: String
               , _role :: Role
               , _talents :: [String]
               } deriving (Show, Eq, Ord)

makeLenses ''CrewMember

-- Name Role Talents
roster :: S.Set CrewMember
roster = S.fromList
    [ CrewMember "Grumpy Roger" Gunner ["Juggling", "Arbitrage"]
    , CrewMember "Long-John Bronze" PowderMonkey ["Origami"]
    , CrewMember "Salty Steve" PowderMonkey ["Charcuterie"]
    , CrewMember "One-eyed Jack" Navigator []
    ]
