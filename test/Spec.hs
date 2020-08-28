import qualified Tests01 as T01
import qualified Tests02 as T02
import qualified Tests03 as T03
import qualified Tests05 as T05

main :: IO ()
main = do
  T01.main
  T02.main
  T03.main
  T05.main