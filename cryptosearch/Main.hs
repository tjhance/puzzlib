import Cryptogram

import System.Environment

data Options = Options { dictionary :: String } deriving (Eq, Show, Ord)
defaultOptions = Options { dictionary = "/usr/share/dict/words" }

main :: IO ()
main = do
    a <- getArgs
    let (opts, args) = parseOpts a defaultOptions
    print opts
    print args

parseOpts :: [String] -> Options -> (Options, [String])
parseOpts [] opts = (opts, [])
parseOpts (a:as) opts = case a of
    "-d" -> parseOpts (tail as) (opts { dictionary = head as })
    x    -> let (r_opts, r_args) = parseOpts as opts in (r_opts, x:r_args)
