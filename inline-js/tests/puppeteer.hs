import Language.JavaScript.Inline.Puppeteer
import Language.JavaScript.Inline.Session

main :: IO ()
main = do
  installPuppeteer Full
  withJSSession defJSSessionOpts $ \s -> do
    p <- newPuppeteer s defPuppeteerLaunchOpts
    userAgent p >>= print
