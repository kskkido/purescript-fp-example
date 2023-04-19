module Main
  ( main
  ) where

import RIO
import qualified System.IO as IO
import qualified Control.Monad.Reader as Reader
import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified Database.PostgreSQL.Simple.SqlQQ as PostgreSQL.SqlQQ
import qualified HonduitDatabase.Lib.ExceptT as ExceptT
import qualified HonduitDatabase.Data.AppConfig as AppConfig
import qualified HonduitDatabase.Data.AppContext as AppContext

main :: IO.IO ()
main = do
  config <- ExceptT.liftFail AppConfig.fromSystem
  context <- AppContext.fromAppConfig config
  void do
    PostgreSQL.execute_ context.database
      [PostgreSQL.SqlQQ.sql|
        CREATE TABLE IF NOT EXISTS users (
          id SERIAL PRIMARY KEY
        );
        CREATE TABLE IF NOT EXISTS user_auth_credentials (
          id SERIAL PRIMARY KEY,
          user_id INTEGER NOT NULL REFERENCES users,
          email TEXT NOT NULL,
          password TEXT NOT NULL
        );
        CREATE TABLE IF NOT EXISTS user_profiles (
          id SERIAL PRIMARY KEY,
          user_id INTEGER NOT NULL REFERENCES users,
          username VARCHAR (255) NOT NULL,
          bio TEXT,
          image TEXT
        );
        CREATE TABLE IF NOT EXISTS user_profile_followers(
          id serial PRIMARY KEY,
          follower_id INTEGER NOT NULL REFERENCES user_profiles,
          following_id INTEGER NOT NULL REFERENCES user_profiles
        );
        CREATE TABLE IF NOT EXISTS tags(
          id serial PRIMARY KEY,
          label VARCHAR(255) UNIQUE
        );
        CREATE TABLE IF NOT EXISTS articles (
          slug TEXT UNIQUE NOT NULL,
          id SERIAL PRIMARY KEY,
          title VARCHAR(255) NOT NULL,
          description TEXT NOT NULL,
          body TEXT NOT NULL,
          created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
          updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
          author_id INTEGER NOT NULL REFERENCES user_profiles
        );
        CREATE TABLE IF NOT EXISTS article_tags(
          id serial PRIMARY KEY,
          article_id INTEGER NOT NULL REFERENCES articles,
          tag_id INTEGER NOT NULL REFERENCES tags
        );
      |]

