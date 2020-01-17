# Functorial Data Migration

An (work in progress) implementation of functorial data migration in SQL as described in "Relational Foundations For Functorial Data Migration" by David I. Spivak and Ryan Wisnesky, 2015, https://math.mit.edu/~dspivak/informatics/relfound.pdf

Functorial data migration is a way to express database schemas, migrations between database schemas, and constraints on data migrations using category theory.

Sure, I could just using the CQL tool developed by the authors of the paper (https://www.categoricaldata.net/), but:

- I hope to get a deeper understanding of the underlying mathematical machinery through developing an implementation.

- I'd like an implementation to be simple to integrate with my mindmapping/knowledge-engineering project https://www.github.com/rskew/workflow, which aims for fluidity between mindmapping and database-backed knowledge-management.

- I want to explore generalisations of mappings to e.g. machine-learning algorithms as mappings between schemas.


## Roadmap

Base types
[x] Represent signatures for categories/schemas and their mappings
[x] Check that a mapping corresponds to a valid functor (a function from objects to objects and a function from morphisms to paths that preserves source, target and composition)

Pre-migration conditions
[ ] Check that a signature corresponds to a finite category (i.e. has cycles)
[ ] Check that a mapping's object function is a bijection (required for well-behaved Pi migrations)
[ ] Check that a mapping is a discrete op-fibration (required for well-behaved Sigma migrations)

SQL generation
[ ] Compile a signature into SQL operations that create the database tables for the corresponding schema
[ ] Compile a functor into SQL operations that implement the Delta migration
[ ] Compile a functor into SQL operations that implement the Sigma migration
[ ] Compile a functor into SQL operations that implement the Pi migration


## Quick start

You will either need [NPM](https://www.npmjs.com/get-npm) or [Yarn](https://yarnpkg.com/) installed.

### NPM

```sh
npm run dev     # run development server
npm run build   # build production assets
```

### Yarn

```sh
yarn dev        # run development server
yarn build      # build production assets
```

## Development

We use [spago](https://github.com/spacchetti/spago) to manage our Purescript dependencies.
While this is installed as a `dev-dependency` in `package.json` you may want to install it directly to make it easier to manage these dependencies directly.

## License

The project is licensed under the terms of the Apache License (Version 2.0).

See [LICENSE](./LICENSE) or http://www.apache.org/licenses/LICENSE-2.0 for details.
