module Topology

open QuickGraph
open System.Collections.Generic

//可区分联合 实例化用的
type NodeType = 
   | Start
   | End
   | Outside
   | Inside
   | Unknown of Set<string>
   //结构体定义，与类相似 Topo.GraphIn.T.Node
type Node = 
   struct
      val Loc : string
      val Typ : NodeType
      new(l, t) = 
         { Loc = l
           Typ = t }
   end
//Topo.GraphIn.T 打开了库函数 QucikGraph.BidirectionalGraph
type T = 
   | Topology of BidirectionalGraph<Node, Edge<Node>>
// Topo.Kind类
type Kind = 
   | Concrete
   | Abstract
   | Template
   //Topo.Graph
type GraphInfo = 
   { Graph : T
     Pods : Map<string, Set<string>>
     InternalNames : Set<string>
     ExternalNames : Set<string>
     AsnMap : Map<string, string>
     AsnRevMap : Map<string, string>
     RouterMap : Map<string, string>
     IpMap : Dictionary<string * string, string * string> }

type CustomLabel = 
   | SomeLabel of string
   | AllLabel of string
   | NameLabel of string
// Topo.EdgeLabelInfo.EdgeInfo 
type EdgeInfo = 
   { Label : string
     OtherLabel : string option
     Source : string
     Target : string
     Scope : string
     Front : CustomLabel list
     Back : CustomLabel list }
// Topo.EdgeLabelInfo Map容器<key=字符串*字符串，EdgeInfo列表>
type EdgeLabelInfo = Map<string * string, EdgeInfo list>

type TopoInfo = 
   class
      val NetworkAsn : int
      val Kind : Kind
      val ConcreteGraphInfo : GraphInfo
      val AbstractGraphInfo : GraphInfo
      val EdgeLabels : EdgeLabelInfo
      val NodeLabels : Map<string, string>
      val PodLabels : Set<string>
      val EnclosingScopes : Map<string, string list>
      val Concretization : Map<string, Set<string>>
      val Abstraction : Map<string, string>
      val Constraints : List<string>
      val TemplateVars : Map<string option * string, Set<int * int * int * int * int>>
      
      new(nasn, k, cg, ag, els, nls, pls, escopes, con, abs, cs, tvs) = 
         { NetworkAsn = nasn
           Kind = k
           ConcreteGraphInfo = cg
           AbstractGraphInfo = ag
           EdgeLabels = els
           NodeLabels = nls
           PodLabels = pls
           EnclosingScopes = escopes
           Concretization = con
           Abstraction = abs
           Constraints = cs
           TemplateVars = tvs }
      
      member this.SelectGraphInfo = 
         match this.Kind with
         | Concrete -> this.ConcreteGraphInfo
         | Abstract | Template -> this.AbstractGraphInfo
      
      member this.IsTemplate = 
         match this.Kind with
         | Concrete -> false
         | Abstract -> false
         | Template -> true
   end