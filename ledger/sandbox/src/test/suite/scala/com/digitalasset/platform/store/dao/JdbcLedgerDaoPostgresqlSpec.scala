// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.platform.store.dao

import org.scalatest.{AsyncFlatSpec, Matchers}

// Aggregate all specs in a single run to not start a new database fixture for each one
final class JdbcLedgerDaoPostgresqlSpec
    extends AsyncFlatSpec
    with Matchers
    with JdbcLedgerDaoSuite
    with JdbcLedgerDaoBackendPostgresql
    with JdbcLedgerDaoCompletionsSpec
    with JdbcLedgerDaoConfigurationSpec
    with JdbcLedgerDaoContractKeysSpec
    with JdbcLedgerDaoContractsSpec
    with JdbcLedgerDaoDivulgenceSpec
    with JdbcLedgerDaoFetchSerializationSpec
    with JdbcLedgerDaoLedgerEntriesSpec
    with JdbcLedgerDaoPackagesSpec
    with JdbcLedgerDaoPartiesSpec
    with JdbcLedgerDaoTransactionsSpec
    with JdbcLedgerDaoTransactionTreesSpec