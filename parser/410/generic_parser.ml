
let interface buf =
    (Parse.interface buf)
    |> Migrate_parsetree.Migrate_410_411.copy_signature
    |> Migrate_parsetree.Migrate_411_412.copy_signature

let implementation buf =  (Parse.implementation buf)
   |> Migrate_parsetree.Migrate_410_411.copy_structure
   |> Migrate_parsetree.Migrate_411_412.copy_structure

let expression buf =
    (Parse.expression buf)
   |> Migrate_parsetree.Migrate_410_411.copy_expression
   |> Migrate_parsetree.Migrate_411_412.copy_expression

