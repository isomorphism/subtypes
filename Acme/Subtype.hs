{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- | Module     : Acme.Subtype
-- Copyright    : (c) 2012 C. A. McCann
-- License      : WTFPL
-- 
-- Maintainer   : cam@uptoisomorphism.net
-- Stability    : like a rock
-- Portability  : like a rock
--
-- This module makes explicit the subtyping system supplied by GHC Haskell,
-- and defines the types 'Top' and 'Bottom' representing the greatest and
-- least element of the subtyping relation, respectively.
-- 
-----------------------------------------------------------------------------

module Acme.Subtype ( Top(), Bottom()
                    , getTop, getBottom
                    , (:<:)(..)
                    ) where


-- | The 'Top' type represents the terminal object in the category of types and
--   valid substitutions, as values of any other type may be safely upcast to 
--   'Top' for use with functions operating on 'Top' values, without loss of 
--   information.
--   
--   The 'getTop' accessor has been helpfully provided for accessing the 
--   information contained in a value of type 'Top'. 
data Top = forall a. Top { getTop :: a }

instance Show Top where show _ = "⊤"



-- | The 'Bottom' type represents the initial object in the category of types 
--   and valid substitutions, as values of the type 'Bottom' may be safely 
--   upcast to any other type.
newtype Bottom = Bottom { _getBottom :: forall a. a }

getBottom :: Bottom -> a
getBottom = _getBottom

instance Show Bottom where show _ = "⊥"


-- | The subtype constraint @a ':<:' b@ indicates that @a@ is a subtype of @b@,
--   allowing a conversion provided by the 'upcast' function. A 'downcast'
--   function is also provided for types in contravariant position.
--   
--   The notions of co- and contravariance are of course familiar to 
--   experienced users of object-oriented languages, as the concepts are 
--   fundamental to subtyping and therefore well-supported by any practical
--   OOP language. Haskell programmers may, however, find them somewhat 
--   unintuitive. For a brief description that may be more approachable than 
--   the complexity imposed by OOP languages, see here: <http://ncatlab.org/nlab/show/contravariant+functor>
--
--   Minimal complete definition: 'upcast'.
--
--   Subtyping relations should satisfy the following properties:
--
--   * Any operation defined on both types should behave identically for @x@ 
--     and @upcast x@.
--
--   * Given @a :<: b@, @a :<: c@, @b :<: d@, and @c :<: d@, upcasting from 
--     @a@ to @d@ should produce the same result whether cast via @b@ or @c@.
--
--   * If @x@ and @y@ are distinct values of the same type, @upcast x@ and 
--     @upcast y@ should be distinct values for any supertype (although it is
--     not required that there be a means of distinguishing them).
class a :<: b where
    -- | @'upcast' x@ applies a covariant substitution, converting @x@ from the
    --   type @a@ to its supertype @b@.
    upcast   :: a -> b
    
    -- | @'downcast' f@ applies a contravariant substitution to the argument of
    --   the function @f@.
    downcast :: (b -> r) -> (a -> r)
    downcast = (. upcast)

instance Bottom :<: a where
    upcast = getBottom

instance a :<: Top where
    upcast = Top


