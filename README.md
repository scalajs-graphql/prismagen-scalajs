# prismagen-scalajs

Generates prisma SDK from `prisma.graphql` file.

## Usage

```scala

//install

yarn global add prismagen-scalajs 
or 
npm install -g  prismagen-scalajs


//run

prismagen-scalajs --schema ./data/prisma.graphql --output src/main/scala/app/server/db

or 

prismagen-scalajs --s ./data/prisma.graphql --o src/main/scala/app/server/db 

```
