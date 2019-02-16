
# Online Business Card application

## Build

- Get Stack, make
- `stack setup` to get GHCJS etc
- `make all`

## View locally

- ` make serve `

  This command makes use of `sws` - a simple Haskell web server - to serve static content from the `dist` directory. If you have no `sws` installed feel free to use any other preferred web server serving files from the same directory.

## Use for yourself 

- Copy the contents of the `dist` directory to your web server path.
- Create a Github Gist containing a json representation of the `Data.Tree` site tree (or use the example below.). 
- Replace a root gist id in the `index.html` file with the id of your root gist:

  `<script>window.RootG=JSON.stringify({"rootGist":"-your-gist-id-here-"})</script>`
- Open the page in a browser.


## Example root gist

```json

[
    [
        {
            "dataSource": "<a gist id here>",
            "title": "Menu 1",
            "path": "menu1"
        },
        []
    ],
    [
        {
            "dataSource": "<another gist id here>",
            "title": "Menu 2",
            "path": "menu2"
        },
        []
    ]
]P

```
