package org.scala_libs.scuartz

import java.util.Date
import org.quartz.{Job,JobDetail,JobExecutionContext,Scheduler,SimpleTrigger,Trigger,CronExpression}

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
  
  class CronSubExpr[T <: TimeUnit](val rangeSet: Set[Range]) (implicit val timeUnit: T) {
    
    override def toString =
      if (rangeSet isEmpty)
        timeUnit.toStringEmpty
      else
        rangeSet map { r =>
          if (r.end > timeUnit.maxValue)
            throw new IllegalArgumentException("Maximum value for time unit " + timeUnit.getClass +
                                               " exceeded: " + r.end)
          if (r.start < timeUnit.minValue)
            throw new IllegalArgumentException("Minimum value for time unit " + timeUnit.getClass +
                                               " exceeded: " + r.start)
          val step = if (r.step == 1) "" else "/" + r.step
          val end = if (r.start == r.end) "" else "-" + r.end
          r.start + end + step
        } mkString ","
  }
  
  trait TimeUnit {
    val minValue: Int
    val maxValue: Int
    def toStringEmpty = "*"
  }
  
  trait Second extends TimeUnit {
    val minValue = 0
    val maxValue = 59
  }
  implicit object Second extends Second
  
  trait Minute extends TimeUnit {
    val minValue = 0
    val maxValue = 59
  }
  implicit object Minute extends Minute
  
  trait Hour extends TimeUnit {
    val minValue = 0
    val maxValue = 23
  }
  implicit object Hour extends Hour
  
  trait DayOfMonth extends TimeUnit {
    val minValue = 1
    val maxValue = 31
  }
  implicit object DayOfMonth extends DayOfMonth
  
  trait Month extends TimeUnit {
    val minValue = 1
    val maxValue = 12
  }
  implicit object Month extends Month
  
  trait DayOfWeek extends TimeUnit {
    val minValue = 1
    val maxValue = 7
    override def toStringEmpty = "?"
  }
  implicit object DayOfWeek extends DayOfWeek
  
  trait Year extends TimeUnit {
    // according to quartz documentation
    val minValue = 1970
    val maxValue = 2099
    override def toStringEmpty = ""
  }
  implicit object Year extends Year
  
  case class Cron(
    seconds: CronSubExpr[Second] = Set(),
    minutes: CronSubExpr[Minute] = Set(),
    hours: CronSubExpr[Hour] = Set(),
    dayOfMonth: CronSubExpr[DayOfMonth] = Set(),
    month: CronSubExpr[Month] = Set(),
    dayOfWeek: CronSubExpr[DayOfWeek] = Set(),
    year: CronSubExpr[Year] = Set()
  ) {
    override def toString = productIterator mkString " "
  }

  implicit def schedulerToRichScheduler (scheduler : Scheduler) = new RichScheduler(scheduler)

  implicit def stringToRichTrigger (jobName : String) = {
    val trigger = new SimpleTrigger(jobName)
    trigger.setJobName(jobName)
    RichTrigger(trigger, None)
  }
  
  implicit def cronToExpression(cron: Cron) = new CronExpression(cron.toString)
  
  implicit def intToCronSubExpr[T <: TimeUnit](i: Int) (implicit timeUnit: T) : CronSubExpr[T] =
    new CronSubExpr[T](Set(i to i)) (timeUnit)
  
  implicit def intToRange(i: Int): Range = i to i
  
  implicit def intSetToRangeSet[T <: TimeUnit](s: Set[Int]) (implicit timeUnit: T) : CronSubExpr[T] =
    new CronSubExpr[T](s map (i => i to i)) (timeUnit)
  
  implicit def rangeToCronSubExpr[T <: TimeUnit](r: Range) (implicit timeUnit: T) : CronSubExpr[T] =
    new CronSubExpr[T](Set(r)) (timeUnit)
  
  implicit def rangeSetToCronSubExpr[T <: TimeUnit,R <: Range](s: Set[R]) (implicit timeUnit: T) : CronSubExpr[T] =
    new CronSubExpr[T](s.asInstanceOf[Set[Range]]) (timeUnit)
  
  implicit def jobClazzToJobInfo[T <: Job](clazz: Class[T]) = new JobInfo(clazz)
  
  // unusable because of issue with zero-arg constructors
  // implicit def funcToJobInfo (f : JobExecutionContext ⇒ Unit) = new JobInfo(f)
  // implicit def funcToJobInfo (f : () ⇒ Unit) = 
  //   new JobInfo((ignore : JobExecutionContext) ⇒ f())
}

