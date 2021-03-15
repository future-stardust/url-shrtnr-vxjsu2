# VXJsU2hvcnRlbmVyRmFjdG9yeS5jcmVhdGVXaXRoTWVtYmVycyhqYXZhU3Vja3NGYW5jbGFiKTsK - UWL showtenew { 🥺👉👈 🧪}
<img align="right" width="250px" src="https://user-images.githubusercontent.com/32426842/111226674-25537500-85ea-11eb-8eb4-9a20bd421a6f.png">

## Students gwoup

- [@p3rsik](https://github.com/p3rsik)
- [@dedifferentiator](https://github.com/dedifferentiator)

## Design document
```
.∧＿∧ 
( ･ω･｡)つ━☆・*。 
⊂   ノ    ・゜+. 
しーＪ   °。+ *´¨) 
         .· ´¸.·*´¨) 
        (¸.·´ (¸.·'* ☆ https://github.com/future-stardust/url-shrtnr-vxjsu2/blob/main/README.md
```

### S-System stwuctuwe 😳👉👈
 | Module   | Path                                       | Descwiption
 | -        | -                                          | -
 | Database | [src/Database](src/Database)               | Top-level functions / common ADT-s
 | -        | [src/Database/Tree](src/Database/Tree)     | Base of database owdewable stwuct (B+-twee at the moment) and its functions
 | -        | [src/Database/Url](src/Database/Url)       | `Url` db repwesentation, its instances, quewies OwO
 | -        | [src/Database/User](src/Database/User)     | `User` db repwesentation, its instances, quewies 
 | Sewvew   | [src/Server](src/Server)                   | Sewvew's cowe: auth, uwl-to-showtened-uwl encoding, cowe weexpowts, etc
 | -        | [src/Server/API](src/Server/API)           | Handlews' types accowding to API specs
 | -        | [src/Server/Handlers](src/Server/Handlers) | Handlews of web-sewver
 | -        | [src/Server/Types](src/Server/Types)       | `Url` and `User` wecowds, m-monad twansfowmers, convewsions between types, auxiliawy type aliases
 


## Install :3
`git clone https://github.com/future-stardust/url-shrtnr-vxjsu2.git`

`cd url-shrtnr-vxjsu2`

`stack install`

## Usage
All you have to do is ~~follow the damn twain~~ wun the binawy and web-sewver will stawt listening to some powt, idk which one sempai~~ (should be `8080`), look in `app/.hs`

`$ shortener`

### Sign up
`curl -v -X POST -H "Content-Type: application/json" --data '{"email":"foo","password":"bar"}' http://localhost:8080/users/signup`
### Sign in
`curl -v -X POST -H "Content-Type: application/json" --data '{"email":"foo","password":"bar"}' http://localhost:8080/users/signin`

Thewe will be `JWT-token` in wesponse, use it wisely.
### Cweate Url
##### w/o alias
`curl -v -X POST -H "Authorization: Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiZW1haWwiOiJmb28iLCJwYXNzd29yZCI6ImJhciJ9fQ.LlD2LOFalJ-mwxOBR3AqwyvObgbQ4V3A6H9bHlUFqO7vLVBxv7-uu__6r51R2AwYts71_vE2juzxWpOcCyga9g" -H "Content-Type: application/json" --data '{"url": "http://kek.lol"}' http://localhost:8080/urls/shorten`
##### w/ alias 😈😈🤙🤙🤙
`curl -v -X POST -H "Authorization: Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiZW1haWwiOiJmb28iLCJwYXNzd29yZCI6ImJhciJ9fQ.LlD2LOFalJ-mwxOBR3AqwyvObgbQ4V3A6H9bHlUFqO7vLVBxv7-uu__6r51R2AwYts71_vE2juzxWpOcCyga9g" -H "Content-Type: application/json" --data '{"url": "http://kek.lol", "alias": "arbidol"}' http://localhost:8080/urls/shorten`
### Get rediwected to owiginal uwl
`curl -v http://localhost:8080/r/arbidol`
### Delete uwl
`curl -v -X DELETE -H "Authorization: Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiZW1haWwiOiJmb28iLCJwYXNzd29yZCI6ImJhciJ9fQ.LlD2LOFalJ-mwxOBR3AqwyvObgbQ4V3A6H9bHlUFqO7vLVBxv7-uu__6r51R2AwYts71_vE2juzxWpOcCyga9g" http://localhost:8080/urls/arbidol`
