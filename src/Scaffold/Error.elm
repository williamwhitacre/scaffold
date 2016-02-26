{--

Copyright (c) 2016, William Whitacre
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted
provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions
and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions
and the following disclaimer in the documentation and/or other materials provided with the
distribution.

3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

--}


module Scaffold.Error

  (DecodingFailure, ErrorType (UnknownError, DecoderError, UserError), Error,

  decodingFailure,

  reportError,     reportErrorAndLog,
  unknownError,    unknownErrorAndLog,
  decoderError,    decoderErrorAndLog,
  userError,       userErrorAndLog)

  where

{-| This module contains the internal error type for Gigan and some helper functions for
constructing errors.

# Definitions
@docs DecodingFailure, Error, ErrorType

# Constructors
@docs reportError, reportErrorAndLog, unknownError, unknownErrorAndLog, decoderError, decoderErrorAndLog, userError, userErrorAndLog, decodingFailure

-}


import Scaffold.App exposing (..)


{-| Represents a decoding task which failed. -}
type alias DecodingFailure =
  { source : String
  , reason : String
  }


{-| Type of error that occurred in Scaffold. -}
type ErrorType euser =
  UnknownError
  | DecoderError DecodingFailure
  | UserError euser


{-| Error record. -}
type alias Error euser =
  { error : ErrorType euser
  , desc : String
  }


{-| Given the source string on which decoding was attempted and the reason for the failure,
give a DecodingFailure -}
decodingFailure : String -> String -> DecodingFailure
decodingFailure source reason =
  { source = source, reason = reason }


{-| Report an error. Given an ErrorType and a String describing what went wrong humanly, create
an error record.  -}
reportError : String -> ErrorType euser -> Error euser
reportError desc error =
  { desc = desc
  , error = error
  }


{-| reportError and log to console.  -}
reportErrorAndLog : String -> ErrorType euser -> Error euser
reportErrorAndLog desc error =
  Debug.log "Gigan report Error: " ("\"" ++ desc ++ "\"")
  |> \_ -> reportError desc error


{-| Report an error, the nature of which is not known. -}
unknownError : String -> Error euser
unknownError desc = reportError desc UnknownError


{-| unknownError and log to console. -}
unknownErrorAndLog : String -> Error euser
unknownErrorAndLog desc = reportErrorAndLog desc UnknownError


{-| Report a problem decoding something. -}
decoderError : String -> DecodingFailure -> Error euser
decoderError desc = DecoderError >> reportError desc


{-| decoderError and log to console. -}
decoderErrorAndLog : String -> DecodingFailure -> Error euser
decoderErrorAndLog desc = DecoderError >> reportErrorAndLog desc


{-| Report a user defined error. -}
userError : String -> euser -> Error euser
userError desc = UserError >> reportError desc


{-| userError and log to console. -}
userErrorAndLog : String -> euser -> Error euser
userErrorAndLog desc = UserError >> reportErrorAndLog desc
