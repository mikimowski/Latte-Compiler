module Main where

import System.Environment (getArgs)
import System.IO (IOMode (ReadMode, WriteMode), hGetContents, hPrint, hClose, hPutStrLn, openFile, stderr, stdin)
import System.Exit
import Text.Printf
import System.FilePath.Posix(dropExtension, takeBaseName, takeDirectory)
import System.Process

import ParLatte
import PrintLatte
import ErrM
import Control.Monad

import Frontend
import Backend

-- flags: [opt, verbose]
main :: IO ()
main = do
  args <- getArgs
  case args of
    file:flags -> do
      forM_ flags (\flag -> when (not $ flag `elem` backendFlags) (do
        hPutStrLn stderr $ "invalid flag: " ++ show flag
        exitFailure
        ))
      handle <- openFile file ReadMode
      content <- hGetContents handle
      runCompiler content (dropExtension file) flags
    _ -> putStrLn "Usage: ./latc_x86 path/to/program.lat [opt|verbose]"
    

runCompiler :: String -> String -> BackendFlags -> IO ()
runCompiler sourceCode filepath flags = do
    let program = pProgram (myLexer sourceCode)
    case program of
        Ok programTree -> do
            --putStrLn $ show programTree
            let frontendResult = frontend programTree
            case frontendResult of
              Left error -> do
                hPutStrLn stderr "ERROR"
                hPutStrLn stderr (show error)
                exitFailure
              Right frontendResult@(program', _, _) -> do
                hPutStrLn stderr "OK"
                
                -- putStrLn "############################### AST ###############################"
                -- putStrLn $ printTree program'
                -- putStrLn $ show program'

                -- putStrLn "\n\n############################### Assembly ###############################"
                let assemblyCode = backend flags frontendResult
                -- writeToStdout assemblyCode
                writeToFile assemblyCode filepath
                -- compileFileLocal filepath
                compileFileStudents filepath
                exitSuccess
        Bad error -> do
          hPutStrLn stderr error
          exitFailure
  
    where
      writeToStdout assemblyCode = forM_ assemblyCode putStrLn

      writeToFile assemblyCode filepath = do
          houtfile <- openFile (filepath ++ ".s") WriteMode
          putStrLn filepath
          forM_ assemblyCode (hPutStrLn houtfile)
          hClose houtfile
            
      compileFileStudents filepath = do
          let objFile = filepath ++ ".o"
              assFile = filepath ++ ".s"
              libFile = "lib/latte_lib.o"
          -- # Create .o for program
          let command = "gcc -m32 -c " ++ assFile ++ " -o " ++ objFile
          callCommand command
          -- # Link program.o with latte_lib.o into executable
          let command = "ld -o " ++ filepath ++ " -m elf_i386 " ++ objFile ++ " " ++ libFile ++ " /home/students/inf/PUBLIC/MRJP/lib32/crt?.o -L/home/students/inf/PUBLIC/MRJP/lib32 -lc"
          callCommand command
  
      compileFileLocal filepath = do
          let objFile = filepath ++ ".o"
              assFile = filepath ++ ".s"
              libFile = "lib/latte_lib.o"
          -- # Create .o for program
          let command = "gcc -m32 -c " ++ assFile ++ " -o " ++ objFile
          callCommand command
          -- # Link program.o with latte_lib.o into executable
          let command = "gcc -m32 " ++ objFile ++ " " ++ libFile ++ " -o " ++ filepath
          callCommand command