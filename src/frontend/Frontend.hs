module Frontend where

import TypeChecker
import StaticError
import Common

frontend :: ProgramMeta -> Either StaticError FrontendResult
frontend = runTypeChecker 