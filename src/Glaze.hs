{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Framework for rendering things (or list of things, or things that contain things) that have metadata/header information as well as values.
module Glaze where

import Control.Lens
import Data.Proxy

-- | Glaze is something that knows how to render some header information
-- and given a value, how to render the value.
-- Ie, the value has been glaze with meta information.
data Glaze a r = Glaze
    { glazeRenderedMeta :: r
    , glazeValueRenderer :: a -> r
    } deriving Functor

makeFields ''Glaze

instance Applicative (Glaze a) where
    pure a = Glaze a (const a)
    x <*> y = Glaze
        ((x ^. renderedMeta) (y ^. renderedMeta))
        (\a -> (x ^. valueRenderer) a ((y ^. valueRenderer) a))

-- | Given a wrapping function for the meta and renderer value,
-- and Glaze instructions for a, run them all together.
renderWith :: (r -> r -> b) -> Glaze a r -> a -> b
renderWith mainWrapper rdr a =
    mainWrapper (rdr ^. renderedMeta) ((rdr ^. valueRenderer) a)

-- | Lifts glazing function into an Applicative
glazeA ::
  (Traversable t, Applicative m) =>
  (wrapper -> meta -> t rs -> r) -> m wrapper -> m meta -> t (m rs) -> m r
glazeA f wrapper meta rs = f <$> wrapper <*> meta <*> sequenceA rs

-- | This can be used to make a Glaze for a list of things to render as a table.
-- Given (mainWrapper :: rows -> final render, headerRowWrapper:: fields -> header row, valueRowWrapper -> value row)
-- the rendered meta, and a list of @Glaze a@, transform it to a Glaze for a list of as.
glazeList :: ([row] -> r, [field] -> row, [field] -> row)
    -> r
    -> [Glaze a field]
    -> Glaze [a] r
glazeList (mainWrapper, headerRowWrapper, valueRowWrapper) meta rs =
    Glaze meta glazeList'
  where
      rs' = sequenceA rs
      glazeList' as =
          mainWrapper $
          headerRowWrapper (rs' ^. renderedMeta) :
          (valueRowWrapper . view valueRenderer rs' <$> as)

-- | Applicative version of glazeList
glazeListA ::
  Applicative f =>
  f ([row] -> r, [field] -> row, [field] -> row)
  -> f r -> [f (Glaze a field)] -> f (Glaze [a] r)
glazeListA = glazeA glazeList

-- | This can be used to generate a Glaze for a larger supercomponent.
-- Given (wrapper :: list of (meta, rendered value) to final render)
-- the rendered meta, and a list of @Glaze a@, transform it to a Glaze for a single a.
-- In this case, use 'reglaze' to generate @Glaze a@ for each subcomponent to use as the list of @Glaze a@
glazeFields :: ([(b, b)] -> r) -> r -> [Glaze a b] -> Glaze a r
glazeFields wrapper meta rs =
    Glaze meta (\a -> wrapper $ (\r -> (r ^. renderedMeta, (r ^. valueRenderer) a)) <$> rs)

-- | Applicative versino of glazeFields
glazeFieldsA ::
  Applicative f =>
  f ([(b, b)] -> r) -> f r -> [f (Glaze a b)] -> f (Glaze a r)
glazeFieldsA = glazeA glazeFields

-- | Makes a glaze of the type of a larger component, but only using
-- the information of a smaller component.
-- Given a factory function that can create glazes from Proxy a
-- and a Lens into subcomponent a, create the glaze of the type of the larger component.
reglaze :: (Proxy a -> Glaze a r) -> Getter s a -> Glaze s r
reglaze f lns = Glaze
        (rdr ^. renderedMeta)
        (\s -> (rdr ^. valueRenderer) (s ^. lns))
  where
    rdr = f (Proxy :: Proxy a)

-- | Applicative version of reglaze
reglazeA :: Applicative f => (Proxy a -> f (Glaze a r)) -> Getter s a -> f (Glaze s r)
reglazeA f lns = (\rdr -> Glaze
        (rdr ^. renderedMeta)
        (\s -> (rdr ^. valueRenderer) (s ^. lns))) <$> f (Proxy :: Proxy a)
