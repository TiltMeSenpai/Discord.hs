0.2.x -> 0.3.x: 
- `Fetchable` is now `Fetchable a` (can't unwrap existential types, duh)
- Functor and Comonad instances now exist for Fetchable

0.1.x -> 0.2.x:
- Switch from Req to Wreq (See issue #9)
- Breaking api change to UploadFile, now UploadFile fileName file
  (file arg remains a LBS)
