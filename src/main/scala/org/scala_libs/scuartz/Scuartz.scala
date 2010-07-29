package org.scala_libs.scuartz

import java.util.Date
import org.quartz.{Job,JobDetail,JobExecutionContext,Scheduler,SimpleTrigger,Trigger}

object Scuartz {
  
  class RichScheduler (val underlying : Scheduler) {
    def schedule (trigger : RichTrigger) : RichScheduler = {
      trigger match {
        case RichTrigger(realtrigger, Some(job)) ⇒ 
          underlying.scheduleJob(job.detail, realtrigger)
        case RichTrigger(realtrigger, None) ⇒ 
          underlying.scheduleJob(realtrigger)
      }
      this
    }

    def schedule (trigger : Trigger) : RichScheduler = {
      underlying.scheduleJob(trigger)
      this
    }
  }

  class JobInfo[T  <: Job](clazz: Class[T]) {
    val detail = new JobDetail
    val trigger = new SimpleTrigger
    
    detail.setJobClass(clazz)
    
    def as(name : String) : RichTrigger = {
      detail.setName(name)
      trigger.setName(name)
      trigger.setJobName(name)
      
      // Set a default schedule
      trigger.setStartTime(new Date)
      
      RichTrigger(trigger, Some(this))
    }
  }

  case class RichTrigger(underlying : SimpleTrigger, job : Option[JobInfo[_]]) {
    def at (time : Date) : RichTrigger = {
      underlying.setStartTime(time)
      this
    }

    def at (time : Long) : RichTrigger = at(new Date(time))

    def after (interval : Long) : RichTrigger = at(new Date(System.currentTimeMillis + interval))

    def until (time : Date) : RichTrigger = {
      underlying.setEndTime(time)
      this
    }

    def until (time : Long) : RichTrigger = until(new Date(time))

    def every (interval : Long) : RichTrigger = {
      underlying.setRepeatInterval(interval)
      this
    }

    def repeat (count : Int) : RichTrigger = {
      underlying.setRepeatCount(count)
      this
    }

    def named (name : String) : RichTrigger = {
      underlying.setName(name)
      this
    }
  }

  implicit def schedulerToRichScheduler (scheduler : Scheduler) = new RichScheduler(scheduler)

  implicit def stringToRichTrigger (jobName : String) = {
    val trigger = new SimpleTrigger(jobName)
    trigger.setJobName(jobName)
    RichTrigger(trigger, None)
  }
  
  implicit def jobClazzToJobInfo[T <: Job](clazz: Class[T]) = new JobInfo(clazz)
  
  // unusable because of issue with zero-arg constructors
  // implicit def funcToJobInfo (f : JobExecutionContext ⇒ Unit) = new JobInfo(f)
  // implicit def funcToJobInfo (f : () ⇒ Unit) = 
  //   new JobInfo((ignore : JobExecutionContext) ⇒ f())
}

