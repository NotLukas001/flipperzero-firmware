/**
 * @file cli_i.h
 * Internal API for getting commands registered with the CLI
 */

#pragma once

#include <furi.h>
#include <m-bptree.h>
#include "cli.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    const char* name; //<! Command name
    void* context; //<! Context passed to callbacks
    CliExecuteCallback execute_callback; //<! Callback for command execution
    CliCommandFlag flags;
} CliCommand;

#define CLI_COMMANDS_TREE_RANK 4

// -V:BPTREE_DEF2:1103
// -V:BPTREE_DEF2:524
BPTREE_DEF2(
    CliCommandTree,
    CLI_COMMANDS_TREE_RANK,
    FuriString*,
    FURI_STRING_OPLIST,
    CliCommand,
    M_POD_OPLIST);
#define M_OPL_CliCommandTree_t() BPTREE_OPLIST(CliCommandTree, M_POD_OPLIST)

ARRAY_DEF(CommandCompletions, FuriString*, FURI_STRING_OPLIST); // -V524
#define M_OPL_CommandCompletions_t() ARRAY_OPLIST(CommandCompletions)

bool cli_get_command(Cli* cli, FuriString* command, CliCommand* result);

void cli_lock_commands(Cli* cli);

void cli_unlock_commands(Cli* cli);

/**
 * @warning Surround calls to this function with `cli_[un]lock_commands`
 */
CliCommandTree_t* cli_get_commands(Cli* cli);

#ifdef __cplusplus
}
#endif
