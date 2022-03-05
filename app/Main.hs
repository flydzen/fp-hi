import           Control.Monad.IO.Class   (liftIO)
import           Data.Set                 (fromList)
import           HW3.Action               (HIO (runHIO), HiPermission (AllowRead, AllowTime, AllowWrite))
import           HW3.Evaluator            (eval)
import           HW3.Parser               (parse)
import           HW3.Pretty               (prettyValue)
import           System.Console.Haskeline

permissions = fromList [AllowRead, AllowWrite, AllowTime]

main :: IO ()
main = runInputT defaultSettings loop where
    loop :: InputT IO ()
    loop = do
        minput <- getInputLine "hi> "
        case minput of
            Nothing     -> return ()
            Just "quit" -> return ()
            Just input  -> do
                case parse input of
                    Left err -> do
                        outputStrLn $ show err
                        loop
                    Right parsed -> do
                        let evalved = eval parsed
                        res <- liftIO $ runHIO evalved permissions
                        case res of
                            Left err -> do
                                outputStrLn $ show err
                                loop
                            Right ok -> do
                                outputStrLn $ show $ prettyValue ok
                                loop
