package scala.runtime

import scala.collection.mutable._

object Instrument {
  type FunctionName = StackTraceElement
  type CallSite = StackTraceElement

  lazy val morphCounts : Map[FunctionName, Map[CallSite, Int]] = 
    Map() withDefault (_ => Map() withDefaultValue 0)
  
  val T = 10
    
  var cnt = 0
  var executing = false
  def getCode(): Unit = {
    if (executing)
      return
    executing = true
    /**
     * higherOrderFun - higher order function
     * callSite - each callsite passes a different function as a parameter
     */
    val Array(higherOrderFun, callSite) = Thread.currentThread.getStackTrace drop 2 take 2 
    val callsPerSite = morphCounts(higherOrderFun)
    callsPerSite(callSite) = callsPerSite(callSite) + 1 
    morphCounts(higherOrderFun) = callsPerSite 

    cnt = cnt + 1
    if (cnt == 100000) {
      for ((k, v) <- morphCounts.toList sortBy (_._2 size)) {
        println(v.size + " -- " + k.getMethodName + " @ " + k.getClassName )
        val counts = v.toList sortBy (-_._2) map (_._2)
        
        val sum:Double = counts.fold(0)((s, c) => s + c)
        val sum10:Double = (counts take T).fold(0)((s, c) => s + c)
        
        println("\t\t" + (counts take T) + ", " + (100*sum10/sum) + "%")
        println("\t\t" + (v.toList sortBy (-_._2) take 3 map (_._1)))
      }
      println()
      cnt = 0
    }
    executing = false
  }
}