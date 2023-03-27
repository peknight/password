package com.peknight.password.domain

case class PasswordOption(length: BoundedLengthInterval, repeat: Boolean, consecutive: Option[Consecutive])
