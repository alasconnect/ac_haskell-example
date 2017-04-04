Tisch uses a large number of language extensions. This is a good thing, however,
as it allows the library to use various advanced Haskell type features to give a
high level of type safety.

> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE OverloadedLabels #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE TypeFamilies #-}

> module Main where

> import Control.Arrow
>   ( returnA )
> import Control.Lens
>   ( Wrapped
>   , Unwrapped
>   , iso
>   , view
>   , _Wrapped'
>   )
> import Control.Monad.Catch
>   ( MonadThrow )
> import Control.Monad.IO.Class
>   ( MonadIO
>   , liftIO
>   )
> import Data.Aeson
>   ( FromJSON
>   , ToJSON
>   )
> import Data.Int
>   ( Int32 )
> import Data.Text
>   ( Text )
> import Data.Time.Clock
>   ( UTCTime )
> import qualified Database.PostgreSQL.Simple as PGS
>   ( connectDatabase
>   , connectPassword
>   , connectUser
>   , defaultConnectInfo
>   )
> import Tisch

Concrete Column Types

Universal empty database type for primary database.
Note that this type has no constructors, it is only good for creating
concrete type associations and nothing else.

> data Db1

All the newtype wrappers for a User data type.

While not absolutely necessary, they allow us to build concrete types with
which to constrain our application by.

While there is a bit of boilerplate involved, it's purpose is to simply give
Tisch the power to understand our types.

UserId is a wrapped Int32.
It uses StandaloneDeriving to get ToJSON and FromJSON wrappers for free.

> newtype UserId = UserId { unUserId :: Int32 }
>   deriving (Show, Eq, Ord, ToJSON, FromJSON)

Automatic Wrapping/Unwrapping instances.

> instance Wrapped UserId where
>   type Unwrapped UserId = Int32
>   _Wrapped' = iso unUserId UserId

This creates the specific PostgreSQL type our type is formatted into.

> instance PgTyped UserId where
>   type PgType UserId = PGInt4

Tisch boilerplate

> instance ToKol UserId UserId

Allow this type to be compared with PG equality.

> instance PgEq UserId

Tisch (Opaleye) type conversion handling.

> instance QueryRunnerColumnDefault PGInt4 UserId where
>   queryRunnerColumnDefault = qrcWrapped

Example of another field being wrapped.

> newtype Email = Email { unEmail :: Text }
>   deriving (Show, Eq, ToJSON, FromJSON)
> instance Wrapped Email where
>   type Unwrapped Email = Text
>   _Wrapped' = iso unEmail Email
> instance PgTyped Email where
>   type PgType Email = PGText
> instance ToKol Email Email
> instance PgEq Email
> instance QueryRunnerColumnDefault PGText Email where
>   queryRunnerColumnDefault = qrcWrapped

Table Relationships

First we declare an empty type which has no constructors to associate with.

> data TUser

This type then get used to create a new type (with the same name) but is
constrained to Table.

> data instance Table TUser      = TUser

This constrains our TUser type to the Database Db1. Db1 is also an empty type
and simply allows us to make these kinds of restrictions. If we had another
DB to connect to we could use it as a separate constraint, giving us the
power to make sure we are using the correct tables/databases with the correct
connection objects, etc.

> type instance Database TUser   = Db1

Same with the DB Schema.

> type instance SchemaName TUser = "public"

And define the actual table name.

> type instance TableName TUser  = "users"

Finally create our column associations.

The Column strings actually get converted to symbols which allows for type
safe checking of them in the form of #field. This uses the OverloadedLabels
language extension.

Next is the write type. 'W means the field cannot be null, 'WD means use the
default value the database provides for the field.

'R means simply read the value as is, while a 'RN means the value could be
null (and therefore a Maybe value).

The next field is the actual PostgreSQL type, although we can do some
special type wrapping to make these concrete Haskell types instead.
(For an example see User.Types)

The final field is the concrete Haskell type we read data out of the database
into, or vice versa.

While it's not necessary to wrap all of the types and just use generic types,
we wrap them to get that extra level of type safety. We don't want to
accidentally update a user with a WorkId.

The last field is an example of simply using the PG type and it's associated
Haskell type.

> type instance Columns TUser =
>   [ 'Column "user_id"   'WD 'R  UserId        UserId
>   , 'Column "email"     'W  'R  Email         Email
>   , 'Column "logged_in" 'WD 'R  PGTimestamptz UTCTime
>   ]

Contrived example of getting a UserId out of a (HsR TUser).
This would more than likely instantiate a full JSON User type or something.

> hsrUserId :: HsR TUser -> UserId
> hsrUserId = view #user_id

Instantiating an (HsI TUser) to insert into the database.
This would more than likely be a full JSON User type passed in.

> userDataToHsI :: Email -> HsI TUser
> userDataToHsI e =
>   mkHsI TUser
>     (hsi #user_id   WDef) -- WDef tells the database to use the db write default.
>     (hsi #email     e)
>     (hsi #logged_in WDef)

User Queries

Generate type safe SQL using "arrow notation". This notation allows data to
be constrained into actual graph dependencies, and lends itself to type
checking.

Build a type safe SQL query to get a User by their UserId.

> userQueryById :: UserId -> Query Db1 () (PgR TUser)
> userQueryById uid = proc () -> do
>   -- Generate a basic "SELECT * FROM users" query
>   u <- query TUser -< ()
>   -- And restrict it with "WHERE user_id = ?"
>   restrict -< eq (#user_id u) (kol uid)
>   -- Use Arrow return to send back the query
>   returnA -< u

Do the actual IO query against the database.
MonadIO and MonadThrow constrain our function to this mtl-style monad
transformer stack.
Conn' means accept a connection with all attributes (read, write, etc).

> userFetch :: (MonadIO m, MonadThrow m) => UserId -> Conn' -> m (Maybe (HsR TUser))
> userFetch uid conn = runQuery1 conn (userQueryById uid)

> runTisch :: IO ()
> runTisch = do
>   conn <- connect PGS.defaultConnectInfo
>           { PGS.connectUser     = "db"
>           , PGS.connectPassword = "user"
>           , PGS.connectDatabase = "pass" }
>   u <- userFetch (UserId 1) conn
>   case u of
>     Nothing -> putStrLn "No User"
>     Just u' -> putStrLn $ "User: " ++ show u'

> main :: IO ()
> main = runTisch
