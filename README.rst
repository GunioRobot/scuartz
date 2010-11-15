Scuartz
=======

This is a set of Scala wrappers and DSL around the Quartz scheduling library.

Cron DSL
--------

If you've had trouble remembering which column in the cron expression corresponds to what, this DSL is for you. Using it, you can just say ``Cron(hours=12)`` to schedule an event at 12 o'clock every day- the default for the unspecified time units is the ``*`` wildcard.

You can also use ranges like ``Cron(month=3 to 6)`` or a set of values like ``Cron(minutes=Set(15,20,30))``. You can even use a step like ``Cron(dayOfMonth=1 by 5)`` meaning every 5th day starting with the 1st.

For week days and months you can use three-letter abbreviations so that you can tell easily which one you're talking about- isn't ``Cron(dayOfWeek=Mon to Fri)`` or ``Cron(month=Set(Mar,Apr,May))`` more readable?

Finally, if you try to construct a cron sub-expression which not among the values allowed for the specified time unit, you will get an ``IllegalArgumentException``.