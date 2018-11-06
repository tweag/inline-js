import Language.JavaScript.Inline.Puppeteer
import Language.JavaScript.Inline.Session

main :: IO ()
main = do
  installPuppeteer
  withJSSession defJSSessionOpts $ \s -> do
    p <- getDefPuppeteerOpts >>= newPuppeteer s
    userAgent p >>= print
