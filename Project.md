
1. Syntax A.

```
test:unit :: file = * -> yarn test $file.spec.ts
test:e2e :: file = * -> docker start db && yarn test $file.e2e.ts
```