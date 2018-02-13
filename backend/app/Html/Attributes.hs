module Html.Attributes 
  ( crossorigin
  , integrity
  , role
  )
where

import Text.Blaze (Attribute, AttributeValue, customAttribute)

integrity :: AttributeValue -> Attribute
integrity = customAttribute "integrity" 

crossorigin :: Attribute
crossorigin = customAttribute "crossorigin" "anonymous"

role :: AttributeValue -> Attribute
role = customAttribute "role" 