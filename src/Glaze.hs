{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

-- | Framework for rendering things (or list of things, or things that contain things) that have metadata/header information as well as values.
module Glaze where

import Control.Lens
import Data.Proxy

-- | Glaze is something that knows how to render some header information
-- and given a value, how to render the value.
-- Ie, the value has been glazeed with meta information.
data Glaze a r = Glaze
    { renderedMeta :: r
    , valueRenderer :: a -> r
    } deriving (Functor)

-- FIXME: is this lawful?
instance Applicative (Glaze a) where
    pure a = Glaze a (const a)
    x <*> y = Glaze
        (renderedMeta x $ renderedMeta y)
        (\a -> valueRenderer x a $ valueRenderer y a)

-- | Given a wrapping function for the meta and renderer value,
-- and Glaze instructions for a, run them all together.
unglazeWith :: (r -> r -> b) -> Glaze a r -> a -> b
unglazeWith mainWrapper rdr a =
    mainWrapper (renderedMeta rdr) (valueRenderer rdr a)

-- | Lifts glazing function into an Applicative
-- This is used to transform functions like 'glazeList' into 'glazeListA'
glazedA
    :: (Traversable t, Applicative m)
    => (wrapper -> meta -> t rs -> r) -> m wrapper -> m meta -> t (m rs) -> m r
glazedA f wrapper meta rs = f <$> wrapper <*> meta <*> sequenceA rs

-- | This can be used to make a Glaze for a list of things to render as a table.
-- Given (mainWrapper :: rows -> final render, headerRowWrapper:: fields -> header row, valueRowWrapper -> value row)
-- the rendered meta, and a list of @Glaze a@ columns to use, transform it to a Glaze for a list of as.
glazeList :: ([row] -> r, [field] -> row, [field] -> row) -- how to merge subresults
    -> r -- meta for the final glaze
    -> [Glaze a field] -- descriptions of columns to use
    -> Glaze [a] r -- know how to render list of as.
glazeList (mainWrapper, headerRowWrapper, valueRowWrapper) meta rs =
    Glaze meta glazeList'
  where
    -- rs' :: Glaze a [field]
    rs' = sequenceA rs
    glazeList' as =
        mainWrapper $
        headerRowWrapper (renderedMeta rs') :
        (valueRowWrapper . valueRenderer rs' <$> as)

-- | Applicative version of glazeList
glazeListA
    :: Applicative f
    => f ([row] -> r, [field] -> row, [field] -> row)
    -> f r
    -> [f (Glaze a field)]
    -> f (Glaze [a] r)
glazeListA = glazedA glazeList

-- | This can be used to generate a Glaze for a larger supercomponent.
-- Given (wrapper :: list of (meta, rendered value) to final render)
-- the rendered meta, and a list of @Glaze a@ field to use, transform it to a Glaze for a single a.
-- In this case, use 'reglaze' to generate @Glaze a@ for each subcomponent to use as the list of @Glaze a@
glazeFields :: ([(b, b)] -> r) -- how to merge results
    -> r -- meta for this glaze
    -> [Glaze a b] -- description of sub components to use
    -> Glaze a r -- know how to render the composite a
glazeFields wrapper meta rs =
    Glaze meta (\a -> wrapper $ (\r -> (renderedMeta r, valueRenderer r a)) <$> rs)

-- | Applicative versino of glazeFields
glazeFieldsA
    :: Applicative f
    => f ([(b, b)] -> r) -> f r -> [f (Glaze a b)] -> f (Glaze a r)
glazeFieldsA = glazedA glazeFields

-- | Makes a glaze of the type of a larger component, but only using
-- the information of a smaller component.
-- Given a Glaze and Lens into subcomponent a, create the glaze of the type of the larger component.
reglaze :: Glaze a r -> Getter s a -> Glaze s r
reglaze rdr lns = Glaze
    (renderedMeta rdr)
    (\s -> valueRenderer rdr (s ^. lns))

-- | Applicative version of reglaze
reglazeA :: Applicative f => f (Glaze a r) -> Getter s a -> f (Glaze s r)
reglazeA rdr lns = (\rdr' -> Glaze
        (renderedMeta rdr')
        (\s -> valueRenderer rdr' (s ^. lns))) <$> rdr
