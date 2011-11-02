/*
 *  ____    ____    _____    ____    ___     ____ 
 * |  _ \  |  _ \  | ____|  / ___|  / _/    / ___|        Precog (R)
 * | |_) | | |_) | |  _|   | |     | |  /| | |  _         Advanced Analytics Engine for NoSQL Data
 * |  __/  |  _ <  | |___  | |___  |/ _| | | |_| |        Copyright (C) 2010 - 2013 SlamData, Inc.
 * |_|     |_| \_\ |_____|  \____|   /__/   \____|        All Rights Reserved.
 *
 * This program is free software: you can redistribute it and/or modify it under the terms of the 
 * GNU Affero General Public License as published by the Free Software Foundation, either version 
 * 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See 
 * the GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License along with this 
 * program. If not, see <http://www.gnu.org/licenses/>.
 *
 */
package com.reportgrid.quirrel

import edu.uwm.cs.gll.LineStream
import edu.uwm.cs.gll.ast.Node

import org.specs2.mutable.Specification

object SolverSpecs extends Specification with parser.Parser with Solver with StubPhases {
  
  "simple expression solution" should {
    "solve left addition" in {
      solve("'a + 2", 'a) must beLike {
        case Some(Sub(_, NumLit(_, "0"), NumLit(_, "2"))) => ok
      }
    }
    
    "solve right addition" in {
      solve("2 + 'a", 'a) must beLike {
        case Some(Sub(_, NumLit(_, "0"), NumLit(_, "2"))) => ok
      }
    }
    
    "solve left subtraction" in {
      solve("'a - 2", 'a) must beLike {
        case Some(Add(_, NumLit(_, "0"), NumLit(_, "2"))) => ok
      }
    }
    
    "solve right subtraction" in {
      solve("2 - 'a", 'a) must beLike {
        case Some(Neg(_, Sub(_, NumLit(_, "0"), NumLit(_, "2")))) => ok
      }
    }
    
    "solve left multiplication" in {
      solve("'a * 2", 'a) must beLike {
        case Some(Div(_, NumLit(_, "0"), NumLit(_, "2"))) => ok
      }
    }
    
    "solve right multiplication" in {
      solve("2 * 'a", 'a) must beLike {
        case Some(Div(_, NumLit(_, "0"), NumLit(_, "2"))) => ok
      }
    }
    
    "solve left division" in {
      solve("'a / 2", 'a) must beLike {
        case Some(Mul(_, NumLit(_, "0"), NumLit(_, "2"))) => ok
      }
    }
    
    "solve right division" in {
      solve("2 / 'a", 'a) must beLike {
        case Some(Div(_, NumLit(_, "2"), NumLit(_, "0"))) => ok
      }
    }
    
    "solve negation" in {
      solve("~'a", 'a) must beLike {
        case Some(Neg(_, NumLit(_, "0"))) => ok
      }
    }
    
    "solve parenthetical" in {
      solve("('a)", 'a) must beLike {
        case Some(NumLit(_, "0")) => ok
      }
    }
  }
  
  "compound binary expression solution" should {
    "solve paired variable addition" in {
      solve("'a + 'a", 'a) must beLike {
        case Some(Div(_, NumLit(_, "0"), NumLit(_, "2"))) => ok
      }
    }
    
    "solve paired variable subtraction" in {
      solve("'a - 'a", 'a) must beLike {
        case Some(NumLit(_, "0")) => ok
      }
    }
    
    "fail to solve paired variable multiplication" in {
      solve("'a * 'a", 'a) must beLike {
        case None => ok
      }
    }
    
    "fail to solve paired variable division" in {
      solve("'a / 'a", 'a) must beLike {
        case None => ok
      }
    }
    
    "solve addition across multiplicand" in {
      solve("'a * 2 + 'a", 'a) must beLike {
        case Some(Div(_, NumLit(_, "0"), NumLit(_, "3"))) => ok
      }
      
      solve("'a + 'a * 2", 'a) must beLike {
        case Some(Div(_, NumLit(_, "0"), NumLit(_, "3"))) => ok
      }
    }
    
    "solve multi-addition across multiplicand" in {
      solve("'a * 2 + 'a + 'a", 'a) must beLike {
        case Some(Div(_, NumLit(_, "0"), NumLit(_, "4"))) => ok
      }
      
      solve("'a + 'a * 2 + 'a", 'a) must beLike {
        case Some(Div(_, NumLit(_, "0"), NumLit(_, "4"))) => ok
      }
      
      solve("'a + 'a + 'a * 2", 'a) must beLike {
        case Some(Div(_, NumLit(_, "0"), NumLit(_, "4"))) => ok
      }
    }
    
    "solve multi-addition parenthetical across multiplicand" in {
      solve("'a * 2 + ('a + 'a)", 'a) must beLike {
        case Some(Div(_, NumLit(_, "0"), NumLit(_, "4"))) => ok
      }
      
      solve("('a + 'a) + 'a * 2", 'a) must beLike {
        case Some(Div(_, NumLit(_, "0"), NumLit(_, "4"))) => ok
      }
    }
    
    "solve addition across dividend" in {
      solve("'a / 2 + 'a", 'a) must beLike {
        case Some(Div(_, Mul(_, NumLit(_, "0"), NumLit(_, "2")), NumLit(_, "3"))) => ok
      }
      
      solve("'a + 'a / 2", 'a) must beLike {
        case Some(Div(_, Mul(_, NumLit(_, "0"), NumLit(_, "2")), NumLit(_, "3"))) => ok
      }
    }
    
    "solve addition of dividends" in {
      solve("'a / 2 + 'a / 3", 'a) must beLike {
        case Some(Div(_, Mul(_, NumLit(_, "0"), Mul(_, NumLit(_, "2"), NumLit(_, "3"))), NumLit(_, "1"))) => ok
      }
      
      solve("'a / 3 + 'a / 2", 'a) must beLike {
        case Some(Div(_, Mul(_, NumLit(_, "0"), Mul(_, NumLit(_, "2"), NumLit(_, "3"))), NumLit(_, "1"))) => ok
      }
    }
    
    "solve subtraction of dividends" in {
      solve("('a * 3) / 2 - 'a / 3", 'a) must beLike {
        case Some(Div(_, Mul(_, NumLit(_, "0"), Mul(_, NumLit(_, "2"), NumLit(_, "3"))), Sub(_, NumLit(_, "9"), NumLit(_, "2")))) => ok
      }
      
      solve("'a / 3 - ('a * 3) / 2", 'a) must beLike {
        case Some(Div(_, Mul(_, NumLit(_, "0"), Mul(_, NumLit(_, "2"), NumLit(_, "3"))), Sub(_, NumLit(_, "9"), NumLit(_, "2")))) => ok
      }
    }
    
    "solve self multiplication of self dividend" in {
      solve("('a / 'a) * 'a", 'a) must beLike {
        case Some(NumLit(_, "0")) => ok
      }
      
      solve("'a * ('a / 'a)", 'a) must beLike {
        case Some(NumLit(_, "0")) => ok
      }
    }
    
    "solve self addition of self dividend" in {
      solve("('a / 'a) + 'a", 'a) must beLike {
        case Some(Sub(_, NumLit(_, "0"), NumLit(_, "1"))) => ok
      }
      
      solve("'a + ('a / 'a)", 'a) must beLike {
        case Some(Sub(_, NumLit(_, "0"), NumLit(_, "1"))) => ok
      }
    }
    
    "solve addition with negation" in {
      solve("2 * 'a + ~'a", 'a) must beLike {
        case Some(NumLit(_, "0")) => ok
      }
      
      solve("~'a + 2 * 'a", 'a) must beLike {
        case Some(NumLit(_, "0")) => ok
      }
    }
    
    "solve multiplication of dividends" in {
      solve("(2 / 'a) * ('a * 'a / 3)", 'a) must beLike {
        case Some(Div(_, Mul(_, NumLit(_, "0"), NumLit(_, "3")), NumLit(_, "2"))) => ok
      }
      
      solve("('a * 'a / 3) * (2 / 'a)", 'a) must beLike {
        case Some(Div(_, Mul(_, NumLit(_, "0"), NumLit(_, "3")), NumLit(_, "2"))) => ok
      }
    }
    
    "solve division of dividends" in {
      solve("(2 / 'a) / (3 / ('a * 'a))", 'a) must beLike {
        case Some(Div(_, Mul(_, NumLit(_, "0"), NumLit(_, "3")), NumLit(_, "2"))) => ok
      }
      
      solve("('a * 'a / 3) / ('a / 2)", 'a) must beLike {
        case Some(Div(_, Mul(_, NumLit(_, "0"), NumLit(_, "3")), NumLit(_, "2"))) => ok
      }
    }
  }
  
  def solve(str: String, id: Symbol): Option[Expr] = {
    val f = solve(parse(LineStream(str))) { case TicVar(_, id2) => id.toString == id2 }
    f(NumLit(LineStream(), "0"))
  }
}