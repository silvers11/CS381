-- Steven Silvers silverss 931 775 590
module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
-- used three letter variables for this section to help get familiar with karel
test :: Test -> World -> Robot -> Bool
test (Not t) wor rob = not (test t wor rob)
test (Facing card) wor (pos, car, i) = car == card
test (Clear dir) wor (pos, card, i) = isClear(neighbor(cardTurn dir card) pos) wor
test Beeper wor (pos, card, i) = hasBeeper pos wor
test Empty wor rob = isEmpty rob

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt Move d w (p, c, i) = let newpos = neighbor c p
                          in if isClear newpos w 
                          	 then OK w (setPos newpos (p, c, i))
                          	 else Error ("Not clear: " ++ show newpos)
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
                              
stmt _ _ _ _ = undefined
    
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
