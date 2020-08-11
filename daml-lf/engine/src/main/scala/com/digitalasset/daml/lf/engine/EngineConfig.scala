// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf.engine

import com.daml.lf.data.NoCopy
import com.daml.lf.{VersionRange, data}
import com.daml.lf.language.{LanguageVersion => LV}
import com.daml.lf.transaction.{TransactionVersion => TV, TransactionVersions}

// FIXME: https://github.com/digital-asset/daml/issues/5164
// Currently only outputTransactionVersions is used.
// languageVersions and outputTransactionVersions should be plug
final case class EngineConfig private (
    // constrains the versions of language accepted by the engine
    allowLanguageVersions: VersionRange[LV],
    // constrains the versions of input transactions
    allowedInputTransactionVersions: VersionRange[TV],
    // constrains the versions of output transactions
    allowedOutputTransactionVersions: VersionRange[TV],
) extends NoCopy {

  private[lf] val allowedInputValueVersions =
    VersionRange(
      TransactionVersions.assignValueVersion(allowedInputTransactionVersions.min),
      TransactionVersions.assignValueVersion(allowedInputTransactionVersions.max),
    )

  private[lf] val allowedOutputValueVersions =
    VersionRange(
      TransactionVersions.assignValueVersion(allowedOutputTransactionVersions.min),
      TransactionVersions.assignValueVersion(allowedOutputTransactionVersions.max),
    )

}

object EngineConfig {

  // Development configuration, should not be used in PROD.
  val Dev: EngineConfig = new EngineConfig(
    allowLanguageVersions = VersionRange(
      LV(LV.Major.V1, LV.Minor.Stable("6")),
      LV(LV.Major.V1, LV.Minor.Dev),
    ),
    allowedInputTransactionVersions = VersionRange(
      TV("10"),
      TransactionVersions.acceptedVersions.last
    ),
    allowedOutputTransactionVersions = TransactionVersions.DevOutputVersions
  )

  // Legacy configuration, to be used by sandbox classic only
  @deprecated("Sandbox_Classic is to be used by sandbox classic only", since = "1.4.0")
  val Sandbox_Classic: EngineConfig = new EngineConfig(
    allowLanguageVersions = VersionRange(
      LV(LV.Major.V0, LV.Minor.Stable("")),
      LV(LV.Major.V1, LV.Minor.Dev),
    ),
    allowedInputTransactionVersions = VersionRange(
      TransactionVersions.acceptedVersions.head,
      TransactionVersions.acceptedVersions.last
    ),
    allowedOutputTransactionVersions = VersionRange(
      TV("10"),
      TransactionVersions.acceptedVersions.last
    )
  )

  def build(
      languageVersions: VersionRange[LV],
      inputTransactionVersions: VersionRange[TV],
      outputTransactionVersions: VersionRange[TV],
  ): Either[String, EngineConfig] = {
    val config = new EngineConfig(
      allowLanguageVersions = languageVersions intersect Dev.allowLanguageVersions,
      allowedInputTransactionVersions = inputTransactionVersions intersect Dev.allowedInputTransactionVersions,
      allowedOutputTransactionVersions = outputTransactionVersions intersect Dev.allowedOutputTransactionVersions,
    )

    Either.cond(
      config.allowLanguageVersions.nonEmpty && config.allowedInputTransactionVersions.nonEmpty && config.allowedOutputTransactionVersions.nonEmpty,
      config,
      "invalid engine configuration"
    )
  }

  def assertBuild(
      languageVersions: VersionRange[LV],
      inputTransactionVersions: VersionRange[TV],
      outputTransactionVersions: VersionRange[TV],
  ): EngineConfig =
    data.assertRight(
      build(
        languageVersions: VersionRange[LV],
        inputTransactionVersions: VersionRange[TV],
        outputTransactionVersions: VersionRange[TV],
      )
    )

  // recommended configuration
  val Stable: EngineConfig = assertBuild(
    languageVersions = VersionRange(
      LV(LV.Major.V1, LV.Minor.Stable("6")),
      LV(LV.Major.V1, LV.Minor.Stable("8")),
    ),
    inputTransactionVersions = VersionRange(TV("10"), TV("10")),
    outputTransactionVersions = VersionRange(TV("10"), TV("10"))
  )

}
