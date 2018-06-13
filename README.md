# servant-dhall - Servant Dhall bindings

![servant](https://raw.githubusercontent.com/haskell-servant/servant/master/servant.png)

Note: accepting dhall expressions from untrusted sources is a security risk.

## Example

Run example

```
cabal new-run example run
```

### Get

```
% curl 'localhost:8000/get'
[ +1, +2, +3, +4, +5, +6, +7, +8, +9, +10 ] : List Integer
```

### Post

```
% curl -D - -X POST -H 'Content-Type: application/x-dhall' --data-raw '[+10,+20]' 'localhost:8000/post'
HTTP/1.1 200 OK
Transfer-Encoding: chunked
Date: Wed, 13 Jun 2018 10:41:07 GMT
Server: Warp/3.2.22
Content-Type: application/x-dhall

[ +11, +21 ] : List Integer
```

### Error case: Unexpected type

```
% curl -D - -X POST -H 'Content-Type: application/x-dhall' --data-raw '[10]' 'localhost:8000/post'
curl: (7) Failed to connect to localhost port 8000: Connection refused
[FL973] ~/Documents/public-haskell/servant-dhall % curl -D - -X POST -H 'Content-Type: application/x-dhall' --data-raw '[10]' 'localhost:8000/post'
HTTP/1.1 400 Bad Request
Transfer-Encoding: chunked
Date: Wed, 13 Jun 2018 10:45:39 GMT
Server: Warp/3.2.22

Expected and actual types don't match : List Integer /= List Natural
```

### Error case: Type error

```
% curl -D - -X POST -H 'Content-Type: application/x-dhall' --data-raw 'True && +1' 'localhost:8000/post'
HTTP/1.1 400 Bad Request
Transfer-Encoding: chunked
Date: Wed, 13 Jun 2018 10:51:52 GMT
Server: Warp/3.2.22

Type error: 
Error: ❰&&❱ only works on ❰Bool❱s

True && +1

(input):1:1
```

### Error case: Imports

As deserialisation is done in pure context type-checking Dhall expressions with
imports is not possible.

```
% curl -D - -X POST -H 'Content-Type: application/x-dhall' --data-raw 'http://example.com/dhall' 'localhost:8000/post'
HTTP/1.1 400 Bad Request
Transfer-Encoding: chunked
Date: Wed, 13 Jun 2018 10:53:52 GMT
Server: Warp/3.2.22

Import found: http://example.com/dhall 
```
