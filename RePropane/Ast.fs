module Ast

open Topology

//Ast.T.Definitions.Position
type Position = 
   { SLine : int
     SCol : int
     ELine : int
     ECol : int }
type Ident = 
   { Pos : Position
     Name : string }

type Expr = 
   { Pos : Position
     Node : Node }
(* 输入的策略要求+拓扑结构
   Input 字符串输入
   TopoInfo 拓扑信息
   Defs 目的地
   CConstraints 
*)
//Ast.T.Definitions.Expr.Node 
and Node = 
   | Ident of Ident * Expr list
   | BlockExpr of (Expr * Expr) list
   | LinkExpr of Expr * Expr
   | DiffExpr of Expr * Expr
   | ShrExpr of Expr * Expr
   | OrExpr of Expr * Expr
   | AndExpr of Expr * Expr
   | LOrExpr of Expr * Expr
   | LAndExpr of Expr * Expr
   | NotExpr of Expr
   | TemplateVar of Ident option * Ident
   | PrefixLiteral of int * int * int * int * (int * int) option
   | CommunityLiteral of int * int
   | Asn of int
   | IntLiteral of int
   | Wildcard
   | True
   | False
//Ast.T.Definitions
type Definitions = Map<string, Position * Ident list * Expr>
type T = 
   { Input : string []
     TopoInfo : Topology.TopoInfo
     Defs : Definitions
     CConstraints : ControlConstraints }
(* 输入的策略要求+拓扑结构
   Ast 本模块定义的T类型
   Policy 策略
   CConstrations
   OrigLocs *)
type PolicyPair = Predicate * Regex.REBuilder * Regex.T list

type PolInfo = 
   { Ast : T
     Policy : PolicyPair list
     CConstraints : CConstraint list
     OrigLocs : Map<Predicate, Set<string>> }