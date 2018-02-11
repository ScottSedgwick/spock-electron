{-# LANGUAGE OverloadedStrings #-}
module Html.Attributes 
  ( crossorigin
  , integrity
  , role
  )
where

import Text.Blaze (Attribute, AttributeValue, customAttribute)

integrity :: AttributeValue -> Attribute
integrity sha = customAttribute "integrity" sha 

crossorigin :: Attribute
crossorigin = customAttribute "crossorigin" "anonymous"

role :: AttributeValue -> Attribute
role value = customAttribute "role" value 