// Learn more about F# at http://fsharp.org

open System.Data.SQLite
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

let app =
    choose
        [ GET >=> choose
            [ path "/hello" >=> OK "Hello GET"
              path "/goodbye" >=> OK "Good bye GET"
            ]
          POST >=> choose
            [ path "/hello" >=> OK "Hello POST"
              path "/goodbye" >=> OK "Good bye POST"
            ]
        ]

let newSQLiteConnection () =
    let connectionString = SQLiteConnectionStringBuilder(DataSource = ":memory:").ToString()
    let conn = new SQLiteConnection(connectionString)
    conn.Open()
    conn


let sqlCreateTable = """
create table `persons` (
    `id` integer primary key,
    `name` text not null,
    `age` integer not null
)
"""

let testSQLite () =
    using (newSQLiteConnection()) (fun connection ->
        using (connection.CreateCommand()) (fun command ->
            command.CommandText <- sqlCreateTable
            command.ExecuteNonQuery() |> ignore
        )
        using (connection.CreateCommand()) (fun command ->
            command.CommandText <- """
                insert into `persons` (`name`, `age`) values ("Yusuke Nojima", 27)
            """
            command.ExecuteNonQuery() |> ignore
        )
        using (connection.CreateCommand()) (fun command ->
            command.CommandText <- """
                select `name`, `age` from `persons`
            """
            using (command.ExecuteReader()) (fun reader ->
                let persons =
                    [ while reader.Read() do
                        yield (reader.GetString(0), reader.GetInt64(1))
                    ]
                printfn "%A" persons
            )
        )
    )

[<EntryPoint>]
let main argv =
    testSQLite()
    startWebServer defaultConfig app
    0 // return an integer exit code
