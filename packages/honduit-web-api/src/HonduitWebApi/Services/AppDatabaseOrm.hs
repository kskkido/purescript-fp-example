module HonduitWebApi.Services.AppDatabaseOrm
  ( AppDatabaseOrm
  , getUserByLoginCredential
  , findUserByLoginCredential
  , getArticlesCountByFilter
  , getArticlesByFilter
  , getTagsByArticleId
  , getUserProfileById
  , findUserProfileById
  , getArticleFavoritesByUserProfileId
  , getAuthUserArticlesByFilter
  , getAuthUserArticlesByFilterByAuthUser
  ) where

import RIO
import qualified RIO.List as List
import qualified System.IO as IO
import qualified Control.Monad.Reader as Reader
import qualified Data.Maybe as Maybe
import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified Database.PostgreSQL.Simple.SqlQQ as PostgreSQL.SqlQQ
import qualified HonduitWebApi.Data.AppDatabaseOrmContext as AppDatabaseOrmContext
import qualified HonduitWebApi.Data.Tag as Tag
import qualified HonduitWebApi.Data.UserLoginCredential as UserLoginCredential
import qualified HonduitWebApi.Data.User as User
import qualified HonduitWebApi.Data.UserProfile as UserProfile
import qualified HonduitWebApi.Data.AuthUser as AuthUser
import qualified HonduitWebApi.Data.AuthUserUserProfile as AuthUserUserProfile
import qualified HonduitWebApi.Data.AuthUserArticle as AuthUserArticle
import qualified HonduitWebApi.Data.AuthUserArticleFeedFilter as AuthUserArticleFeedFilter
import qualified HonduitWebApi.Data.Article as Article
import qualified HonduitWebApi.Data.ArticleFeedFilter as ArticleFeedFilter
import qualified HonduitWebApi.Data.ArticleFavorite as ArticleFavorite

type AppDatabaseOrm m a = Reader.ReaderT AppDatabaseOrmContext.AppDatabaseOrmContext m a

getUserByLoginCredential :: UserLoginCredential.UserLoginCredential -> AppDatabaseOrm IO.IO User.User
getUserByLoginCredential credential = do
  context <- Reader.ask
  muser <- findUserByLoginCredential credential
  case muser of
    Just user -> pure user
    Nothing -> fail "Missing user"

findUserByLoginCredential :: UserLoginCredential.UserLoginCredential -> AppDatabaseOrm IO.IO (Maybe.Maybe User.User)
findUserByLoginCredential credential = do
  context <- Reader.ask
  List.headMaybe <$> liftIO do
    PostgreSQL.query context.database
        [PostgreSQL.SqlQQ.sql|
          SELECT
            user_id as id
          FROM
            user_auth_credentials as u
          WHERE
            email = ? AND
            password = ?
          LIMIT
            1
        |]
        ( credential.email
        , credential.password
        )

getArticlesCountByFilter :: ArticleFeedFilter.ArticleFeedFilter -> AppDatabaseOrm IO.IO Int
getArticlesCountByFilter feedFilter = do
  context <- Reader.ask
  mrow <- List.headMaybe <$> liftIO do
    PostgreSQL.query context.database
      [PostgreSQL.SqlQQ.sql|
        SELECT
          COUNT(*)
        FROM
          ( SELECT
              articles.id as id
            FROM
              articles as articles
            LEFT JOIN
              article_tags as article_tags
              ON
                article_tags.article_id = articles.id
            LEFT JOIN
              user_profiles as user_profiles
              ON
                user_profiles.id = articles.author_id
            LEFT_JOIN
              article_favorites as article_favorites
              ON
                article_favorites.article_id = articles.id
            WHERE
              articles.author_id = ?
            GROUP BY
              articles.id
            HAVING
              ? IN ARRAY_AGG(article_tags.tag_id) AND
              ? IN ARRAY_AGG(article_favorites.user_profile_id)
            ORDER BY DESC
              articles.created_at
          )
      |]
      ( feedFilter.authorId
      , feedFilter.tag
      , feedFilter.favoritedUserProfileId
      )
  lift $ case mrow of
    Just (PostgreSQL.Only a) -> pure a
    _ -> fail "Unable to find count"

getArticlesByFilter :: ArticleFeedFilter.ArticleFeedFilter -> AppDatabaseOrm IO.IO [Article.Article]
getArticlesByFilter feedFilter = do
  context <- Reader.ask
  liftIO do
    PostgreSQL.query context.database
      [PostgreSQL.SqlQQ.sql|
        SELECT
          articles.slug as slug,
          articles.id as id,
          articles.title as title,
          articles.description as description,
          articles.body as body,
          articles.created_at as createdAt,
          articles.updated_at as updatedAt,
          articles.author_id as authorId
        FROM
          articles as articles
        LEFT JOIN
          article_tags as article_tags
          ON
            article_tags.article_id = articles.id
        LEFT JOIN
          user_profiles as user_profiles
          ON
            user_profiles.id = articles.author_id
        LEFT_JOIN
          article_favorites as article_favorites
          ON
            article_favorites.article_id = articles.id
        WHERE
          articles.author_id = ?
        GROUP BY
          articles.slug,
          articles.id,
          articles.title,
          articles.description,
          articles.body,
          articles.created_at,
          articles.updated_at,
          articles.author_id
        HAVING
          ? IN ARRAY_AGG(article_tags.tag_id) AND
          ? IN ARRAY_AGG(article_favorites.user_profile_id)
        ORDER BY DESC
          articles.created_at
        LIMIT
          ?
        OFFSET
          ?
      |]
      ( feedFilter.authorId
      , feedFilter.tag
      , feedFilter.favoritedUserProfileId
      , feedFilter.limit
      , feedFilter.offset
      )

getTagsByArticleId :: Int -> AppDatabaseOrm IO.IO [Tag.Tag]
getTagsByArticleId articleId = do
  context <- Reader.ask
  liftIO do
    PostgreSQL.query context.database
      [PostgreSQL.SqlQQ.sql|
        SELECT
          tags.id as id,
          tags.label as label
        FROM
          article_tags as article_tags
        INNER JOIN
          tags as tags
          ON
            tags.id = article_tags.tag_id
        WHERE
          article_tags.article_id = ?
      |]
      ( PostgreSQL.Only articleId
      )

getUserProfileById :: Int -> AppDatabaseOrm IO.IO UserProfile.UserProfile
getUserProfileById userProfileId = do
  muserProfile <- findUserProfileById userProfileId
  liftIO do
    case muserProfile of
      Just userProfile -> pure userProfile
      Nothing -> fail "Missing user profile"

findUserProfileById :: Int -> AppDatabaseOrm IO.IO (Maybe.Maybe UserProfile.UserProfile)
findUserProfileById userProfileId = do
  context <- Reader.ask
  List.headMaybe <$> liftIO do
    PostgreSQL.query context.database
      [PostgreSQL.SqlQQ.sql|
        SELECT
          user_profiles.id as id,
          user_profiles.user_id as userId,
          user_profiles.username as username,
          user_profiles.bio as bio,
          user_profiles.image as image
        FROM
          user_profiles as user_profiles
        WHERE
          user_profiles.id = ?
      |]
      ( PostgreSQL.Only userProfileId
      )

getUserProfileByUserId :: Int -> AppDatabaseOrm IO.IO UserProfile.UserProfile
getUserProfileByUserId userId = do
  muserProfile <- findUserProfileByUserId userId
  liftIO do
    case muserProfile of
      Just userProfile -> pure userProfile
      Nothing -> fail "Missing user profile"

findUserProfileByUserId :: Int -> AppDatabaseOrm IO.IO (Maybe.Maybe UserProfile.UserProfile)
findUserProfileByUserId userId = do
  context <- Reader.ask
  List.headMaybe <$> liftIO do
    PostgreSQL.query context.database
      [PostgreSQL.SqlQQ.sql|
        SELECT
          user_profiles.id as id,
          user_profiles.user_id as userId,
          user_profiles.username as username,
          user_profiles.bio as bio,
          user_profiles.image as image
        FROM
          user_profiles as user_profiles
        WHERE
          user_profiles.user_id = ?
      |]
      ( PostgreSQL.Only userId
      )

findUserProfileFollowersByFollowerIdByFollowingId :: Int -> Int -> AppDatabaseOrm IO.IO (Maybe.Maybe UserProfile.UserProfile)
findUserProfileFollowersByFollowerIdByFollowingId followerId followingId = do
  context <- Reader.ask
  List.headMaybe <$> liftIO do
    PostgreSQL.query context.database
      [PostgreSQL.SqlQQ.sql|
        SELECT
          user_profile_followers.follower_id as follower_id,
          user_profile_followers.following_id as following_id,
        FROM
          user_profile_followers as user_profile_followers
        WHERE
          user_profile_followers.follower_id = ? AND
          user_profile_followers.following_id = ?
      |]
      ( followerId
      , followingId
      )

getArticleFavoritesByArticleId :: Int -> AppDatabaseOrm IO.IO [ArticleFavorite.ArticleFavorite]
getArticleFavoritesByArticleId articleId = do
  context <- Reader.ask
  liftIO do
    PostgreSQL.query context.database
      [PostgreSQL.SqlQQ.sql|
        SELECT
          article_favorites.id as id,
          article_favorites.article_id as articleId,
          article_favorites.user_profile_id as userProfileId
        FROM
          article_favorites as article_favorites
        WHERE
          article_favorites.article_id = ?
      |]
      ( PostgreSQL.Only articleId
      )

getArticleFavoritesByUserProfileId :: Int -> AppDatabaseOrm IO.IO [ArticleFavorite.ArticleFavorite]
getArticleFavoritesByUserProfileId userId = do
  context <- Reader.ask
  liftIO do
    PostgreSQL.query context.database
      [PostgreSQL.SqlQQ.sql|
        SELECT
          article_favorites.id as id,
          article_favorites.article_id as articleId,
          article_favorites.user_profile_id as userProfileId
        FROM
          article_favorites as article_favorites
        WHERE
          article_favorites.user_profile_id = ?
      |]
      ( PostgreSQL.Only userId
      )

findArticleFavoriteByArticleIdByUserProfileId :: Int -> Int -> AppDatabaseOrm IO.IO (Maybe ArticleFavorite.ArticleFavorite)
findArticleFavoriteByArticleIdByUserProfileId articleId userProfileId = do
  context <- Reader.ask
  List.headMaybe <$> liftIO do
    PostgreSQL.query context.database
      [PostgreSQL.SqlQQ.sql|
        SELECT
          article_favorites.id as id,
          article_favorites.article_id as articleId,
          article_favorites.user_profile_id as userProfileId
        FROM
          article_favorites as article_favorites
        WHERE
          article_favorites.article_id = ? AND
          article_favorites.user_profile_id = ?
      |]
      ( articleId
      , userProfileId
      )

getAuthUserUserProfileById :: Int -> AppDatabaseOrm IO.IO AuthUserUserProfile.AuthUserUserProfile
getAuthUserUserProfileById userProfileId = do
  userProfile <- getUserProfileById userProfileId
  pure $ AuthUserUserProfile.AuthUserUserProfile
    userProfile.id
    userProfile.username
    userProfile.bio
    userProfile.image
    False

getAuthUserUserProfileByIdByAuthUser :: Int -> AuthUser.AuthUser -> AppDatabaseOrm IO.IO AuthUserUserProfile.AuthUserUserProfile
getAuthUserUserProfileByIdByAuthUser userProfileId authUser = do
  authUserProfile <- getUserProfileByUserId authUser.userId
  userProfile <- getUserProfileById userProfileId
  following <- Maybe.isJust <$> findUserProfileFollowersByFollowerIdByFollowingId authUserProfile.id userProfile.id
  pure $ AuthUserUserProfile.AuthUserUserProfile
    userProfile.id
    userProfile.username
    userProfile.bio
    userProfile.image
    following

getAuthUserArticlesByFilter :: AuthUserArticleFeedFilter.AuthUserArticleFeedFilter -> AppDatabaseOrm IO.IO [AuthUserArticle.AuthUserArticle]
getAuthUserArticlesByFilter feedFilter = do
  articles <- getArticlesByFilter $ AuthUserArticleFeedFilter.toArticleFeedFilter feedFilter
  for articles $ \article -> do
    tags <- getTagsByArticleId article.id
    author <- getAuthUserUserProfileById article.authorId
    favorites <- getArticleFavoritesByArticleId article.id
    pure $ AuthUserArticle.AuthUserArticle
      article.slug
      article.title
      article.description
      article.body
      tags
      article.createdAt
      article.updatedAt
      False
      ( length favorites )
      author

getAuthUserArticlesByFilterByAuthUser :: AuthUserArticleFeedFilter.AuthUserArticleFeedFilter -> AuthUser.AuthUser -> AppDatabaseOrm IO.IO [AuthUserArticle.AuthUserArticle]
getAuthUserArticlesByFilterByAuthUser feedFilter authUser = do
  userProfile <- getUserProfileByUserId authUser.userId
  articles <- getArticlesByFilter $ AuthUserArticleFeedFilter.toArticleFeedFilterByAuthUser authUser feedFilter
  for articles $ \article -> do
    tags <- getTagsByArticleId article.id
    author <- getAuthUserUserProfileByIdByAuthUser article.authorId authUser
    favorites <- getArticleFavoritesByArticleId article.id
    favorited <- Maybe.isJust <$> findArticleFavoriteByArticleIdByUserProfileId userProfile.id author.id
    pure $ AuthUserArticle.AuthUserArticle
      article.slug
      article.title
      article.description
      article.body
      tags
      article.createdAt
      article.updatedAt
      favorited
      ( length favorites )
      author

