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
                             else Error ("Blocked at: " ++ show newpos)
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt PutBeeper _ w r = let p = getPos r
                       in if not (isEmpty r)
                          then OK (incBeeper p w) (decBag r)
                          else Error "No beeper to put."
stmt (Turn d) _ w (p, c, i) = OK w (setFacing (cardTurn d c) (p, c, i))
stmt (If check t f) d w r = if test check w r then stmt t d w r else stmt f d w r

stmt (Block []) d w r = OK w r
stmt (Block (h:hs)) d w r = case stmt h d w r of
                            OK wor rob -> stmt (Block hs) d wor rob
                            otherwise -> otherwise
stmt _ _ _ _ = undefined
    
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
