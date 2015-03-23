-- | Src <https://wiki.haskell.org/Poor_man%27s_here_document#Quasiquoting>
module Misc(str,getCurrentTime) where
 
import Language.Haskell.TH
import Language.Haskell.TH.Quote
 
str :: QuasiQuoter
str = QuasiQuoter { quoteExp = stringE }

getCurrentTime :: IO Int
getCurrentTime = undefined
