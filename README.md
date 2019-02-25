
# Online Business Card application

## Build

- Get Stack, make
- `stack setup` to get GHCJS etc
- `make all`

## View locally

- ` make serve `

  This command makes use of `sws` - a simple Haskell web server - to serve static content from the `dist` directory. If you have no `sws` installed feel free to use any other preferred web server serving files from the same directory.

## Use for yourself (no programming needed)

- Copy the contents of the `dist` directory to your web server path.
- Create a Github Gist containing a **json** representation of the `Data.Tree` site tree (or edit the json example below.). 
- Create a gist containing a **json** representation of the blog index.
- Replace the root gist id and the blog gist id in the `index.html` file with the ids of your root gist:

  `<script>window.RootG=JSON.stringify({"rootGist":"...", "blogGist":"..."})</script>`

- Open the page in a browser.


## Example root gist

`dataSource` is a gist id. Gist is expected to contain an **html** string.

`title` is menu item title as shown in the menu.

`path` is slug.


```json

[
    [
        {
            "dataSource": "<gist-id>",
            "title": "Menu item 1",
            "path": "item1"
        },
        [
            [
                {
                    "dataSource": "<gist-id>",
                    "title": "Submenu level 1 item 1",
                    "path": "subitem1"
                },
                []
            ],
            [
                {
                    "dataSource": "<gist-id>",
                    "title": "Submenu level 1 item 2",
                    "path": "subitem2"
                },
                [
                    [
                        {
                            "dataSource": "<gist-id>",
                            "title": "Submenu level 2 item 1",
                            "path": "subsubitem1"
                        },
                        []
                    ],
                    ...
                ]
            ],
            ...
        ]
    ],
    [
        {
            "dataSource": "<gist-id>",
            "title": "Menu item s",
            "path": "item2"
        },
        []
    ],
    ...
]

```


## Example blog index gist

```json
{
    "unblog": [
        {
            "day": 25,
            "month": 2,
            "year": 2019,
            "hash": "<gist-id>",
            "slug": "a-slug",
            "humanTitle": "An article"
        },
        {
            "day": 22,
            "month": 2,
            "year": 2019,
            "hash": "<gist-id>",
            "slug": "b-slug",
            "humanTitle": "Another article"
        },
        ...
    ]
}
```