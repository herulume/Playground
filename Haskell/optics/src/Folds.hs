{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
module Folds where

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

rosterRoles :: Fold (S.Set CrewMember) Role
rosterRoles = folded . role

toListSomehow :: Fold (S.Set CrewMember) Role -> S.Set CrewMember -> [Role]
toListSomehow = flip (^..)

crewRole :: Lens' CrewMember Role
crewRole = role



newtype Name = Name
    { getName :: String
    } deriving Show

data ShipCrew = ShipCrew
    { _shipName :: Name
    , _captain :: Name
    , _firstMate :: Name
    , _conscripts :: [Name]
    } deriving (Show)

makeLenses ''ShipCrew

crewMembers :: Fold ShipCrew Name
crewMembers = folding (\s -> [_captain s, _firstMate s] <> _conscripts s)

crewNames :: Fold ShipCrew Name
crewNames = folding $ \s -> s ^.. captain
                         <> s ^.. firstMate
                         <> s ^. conscripts
myCrew :: ShipCrew
myCrew =
    ShipCrew
        { _shipName = Name "Purple Pearl"
        , _captain = Name "Grumpy Roger"
        , _firstMate = Name "Long-John Bronze"
        , _conscripts = [Name "One-eyed Jack", Name "Filthy Frank"]
        }


names :: [String]
names = myCrew ^.. crewNames . to getName


