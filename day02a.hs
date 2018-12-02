import Data.List
import Control.Arrow
main = interact $ show . checksum . map count . lines
count = (elem 2 &&& elem 3) . map length . group . sort
checksum = (*) <$> length . filter fst <*> length . filter snd
