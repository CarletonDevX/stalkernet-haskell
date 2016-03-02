module Data.Either.Extra ( mapLeft
                         , eitherToMaybe
                         ) where

-- | Bifunctor's 'first'
mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right b) = Right b

-- | Lossy conversion from 'Either' to 'Maybe'
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

-- | Conversion from 'Maybe' to 'Either'
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither def = maybe (Left def) Right

