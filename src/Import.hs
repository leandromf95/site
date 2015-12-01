{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
import Yesod.Static
 
pRoutes = [parseRoutes|
   /use UsuarioR GET POST
   /listar ListUserR GET
   / LoginR GET POST
   /welcome WelcomeR GET
   /logout LogoutR GET
   /admin AdminR GET
|]