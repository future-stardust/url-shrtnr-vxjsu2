# VXJsU2hvcnRlbmVyRmFjdG9yeS5jcmVhdGVXaXRoTWVtYmVycyhqYXZhU3Vja3NGYW5jbGFiKTsK - URL shortener {🧪}


## Students group

- [@p3rsik](https://github.com/p3rsik)
- [@dedifferentiator](https://github.com/dedifferentiator)

## Design document
          
[The Glorious Docs](docs)

### System structure
 | Module   | Path                                       | Description
 | -        | -                                          | -
 | Database | [src/Database](src/Database)               | Top-level functions / common ADT-s
 | -        | [src/Database/Tree](src/Database/Tree)     | Base of database orderable struct (B+-tree at the moment) and its functions
 | -        | [src/Database/Url](src/Database/Url)       | `Url` db representation, its instances, queries
 | -        | [src/Database/User](src/Database/User)     | `User` db representation, its instances, queries
 | Server   | [src/Server](src/Server)                   | Server's core: auth, url-to-shortened-url encoding, core reexports, etc
 | -        | [src/Server/API](src/Server/API)           | Handlers' types according to API specs
 | -        | [src/Server/Handlers](src/Server/Handlers) | Handlers of web-server
 | -        | [src/Server/Types](src/Server/Types)       | `Url` and `User` records, monad transformers, conversions between types, auxiliary type aliases
 


## Install
`git clone https://github.com/future-stardust/url-shrtnr-vxjsu2.git`

`cd url-shrtnr-vxjsu2`

`stack install`

## Usage
All you have to do is ~~follow the damn train~~ run the binary and web-server will start listening to some port, idk which one (should be `8080`), look in `app/.hs`

`$ shortener`

### Sign up
`curl -v -X POST -H "Content-Type: application/json" --data '{"email":"foo","password":"bar"}' http://localhost:8080/users/signup`
### Sign in
`curl -v -X POST -H "Content-Type: application/json" --data '{"email":"foo","password":"bar"}' http://localhost:8080/users/signin`

There will be `JWT-token` in response, use it wisely.
### Create Url
##### w/o alias
`curl -v -X POST -H "Authorization: Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiZW1haWwiOiJmb28iLCJwYXNzd29yZCI6ImJhciJ9fQ.LlD2LOFalJ-mwxOBR3AqwyvObgbQ4V3A6H9bHlUFqO7vLVBxv7-uu__6r51R2AwYts71_vE2juzxWpOcCyga9g" -H "Content-Type: application/json" --data '{"url": "http://kek.lol"}' http://localhost:8080/urls/shorten`
##### w/ alias
`curl -v -X POST -H "Authorization: Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiZW1haWwiOiJmb28iLCJwYXNzd29yZCI6ImJhciJ9fQ.LlD2LOFalJ-mwxOBR3AqwyvObgbQ4V3A6H9bHlUFqO7vLVBxv7-uu__6r51R2AwYts71_vE2juzxWpOcCyga9g" -H "Content-Type: application/json" --data '{"url": "http://kek.lol", "alias": "arbidol"}' http://localhost:8080/urls/shorten`
### Get redirected to original url
`curl -v http://localhost:8080/r/arbidol`
### Delete url
`curl -v -X DELETE -H "Authorization: Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiZW1haWwiOiJmb28iLCJwYXNzd29yZCI6ImJhciJ9fQ.LlD2LOFalJ-mwxOBR3AqwyvObgbQ4V3A6H9bHlUFqO7vLVBxv7-uu__6r51R2AwYts71_vE2juzxWpOcCyga9g" http://localhost:8080/urls/arbidol`
