package org.scala_libs.scuartz

import java.util.Date

import org.quartz.{Job,JobDetail,JobExecutionContext,SimpleTrigger}
import org.quartz.impl.StdSchedulerFactory

import org.specs._
import org.specs.specification._

import Scuartz._

class TestJob extends Job {
  def execute(ctxt : JobExecutionContext) {
    println("test")
  }
}

object CounterJob {
  var counter = 0
}

class CounterJob extends Job {
  def execute(ctx : JobExecutionContext){
    CounterJob.counter += 1
    println(CounterJob.counter)
  }
} 


class ScuartzSpecs extends Specification with DetailedFailures {
  
  "Scuartz" should {
    val sched = StdSchedulerFactory.getDefaultScheduler

    val testJob = new JobDetail("test", classOf[TestJob])
   
    sched.addJob(testJob, true)

    "implicitly convert to RichScheduler as needed" in {
        val trigger = new SimpleTrigger("test", "test", new Date)
        trigger.setJobName("test")
        val ret = sched.schedule(trigger)
        ret must haveClass[RichScheduler]
    }

    "schedule a simple timed job" in {
      (sched.schedule { "test" at (new Date) }).isExpectation
    }

    "schedule a complex timed job" in {
      val now = System.currentTimeMillis
      (sched.schedule { "test" named "test2" at (now + 5000l) every 1000l until (new Date(now + 10000l)) }).isExpectation
    }
    
    "schedule a job from a compliant class" in {
      (sched.schedule {
        classOf[TestJob] as "ticker" every 1000l }).isExpectation
    }
    
    "schedule a closure properly" in {
      sched.start()
      sched.schedule { classOf[CounterJob] as "incrementer" after 1000l every 100l repeat 5 }
      Thread.sleep(3000l)
      sched.shutdown()
      CounterJob.counter must_== 5
    }
    
    "implicitly convert an Int to a Cron sub-expression" in {
      Cron(minutes=15).toString must_== "* 15 * * * ? "
    }

    "implicitly convert a Range to a Cron sub-expression" in {
      Cron(hours=8 to 17).toString must_== "* * 8-17 * * ? "
    }

    "implicitly convert a Set of Ranges to a Cron sub-expression" in {
      Cron(month=Set(4 to 5, 9 to 10)).toString must_== "* * * * 4-5,9-10 ? "
    }

    "implicitly convert a Range by interval to Cron sub-expression" in {
      Cron(seconds=15 to 30 by 5).toString must_== "15-30/5 * * * * ? "
    }
  }
}
