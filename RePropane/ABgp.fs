module ABgp

type T = 
   { PolInfo : Ast.PolInfo
     RConfigs : Map<string, RouterConfig> }

let toConfig (abgp : T)