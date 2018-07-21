{-# LANGUAGE TemplateHaskell #-}
import Control.Lens

data Team = Team { _name :: String, _country :: String, _budget :: Double }
makeLenses ''Team
