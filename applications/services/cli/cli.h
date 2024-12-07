/**
 * @file cli.h
 * API for registering commands with the CLI
 */

#pragma once
#include <furi.h>
#include <m-array.h>
#include "cli_ansi.h"

#ifdef __cplusplus
extern "C" {
#endif

#define RECORD_CLI "cli"

typedef enum {
    CliCommandFlagDefault = 0, /**< Default */
    CliCommandFlagParallelUnsafe = (1 << 0), /**< Unsafe to run in parallel with other apps */
    CliCommandFlagInsomniaSafe = (1 << 1), /**< Safe to run with insomnia mode on */
    CliCommandFlagDontAttachStdio = (1 << 2), /**< Do no attach I/O pipe to thread stdio */
} CliCommandFlag;

/** Cli type anonymous structure */
typedef struct Cli Cli;

/** 
 * @brief CLI execution callbackpointer. Implement this interface and use
 *        `add_cli_command` or `cli_add_command_ex`.
 * 
 * This callback will be called from a separate thread spawned just for your
 * command. The pipe will be installed as the thread's stdio, so you can use
 * `printf`, `getchar` and other standard functions to communicate with the
 * user.
 * 
 * @param [in] pipe     Pipe that can be used to send and receive data. If
 *                      `CliCommandFlagDontAttachStdio` was not set, you can
 *                      also use standard C functions (printf, getc, etc.) to
 *                      access this pipe.
 * @param [in] args     String with what was passed after the command
 * @param [in] context  Whatever you provided to `cli_add_command`
 */
typedef void (*CliExecuteCallback)(FuriPipeSide* pipe, FuriString* args, void* context);

/**
 * @brief Registers a command with the CLI. Provides less options than
 *        `cli_add_command_ex`
 *
 * @param [in] cli       Pointer to CLI instance
 * @param [in] name      Command name
 * @param [in] flags     CliCommandFlag
 * @param [in] callback  Callback function
 * @param [in] context   Custom context
 */
void cli_add_command(
    Cli* cli,
    const char* name,
    CliCommandFlag flags,
    CliExecuteCallback callback,
    void* context);

ARRAY_DEF(CommandCompletions, FuriString*, FURI_STRING_OPLIST); // -V524
#define M_OPL_CommandCompletions_t() ARRAY_OPLIST(CommandCompletions)

/**
 * @brief Command autocomplete callback.
 * 
 * This callback will be called from the shell thread.
 * 
 * @param [in] partial_args Input after the name of the command up to the point
 *                          where TAB was pressed.
 * @param [out] full_args   An initialized empty array that you fill up with
 *                          suggestions for the entire `args`.
 */
typedef void (*CliCompleteCallback)(
    FuriPipeSide* pipe,
    FuriString* partial_args,
    CommandCompletions_t full_args,
    void* context);

/**
 * @brief Extended command descriptor for `cli_add_command_ex`
 */
typedef struct {
    const char* name; //<! Command name
    void* context; //<! Context passed to callbacks
    CliExecuteCallback execute_callback; //<! Callback for command execution
    CliCompleteCallback complete_callback; //<! Callback for command completion. May be `NULL`
    CliCommandFlag flags;
} CliCommand;

/**
 * @brief Registers a command with the CLI. Provides more options than
 *        `cli_add_command`
 * 
 * @param [in] cli       Pointer to CLI instance
 * @param [in] command   Pointer to command descriptor. Not required to be valid
 *                       after this function returns.
 */
void cli_add_command_ex(Cli* cli, CliCommand* command);

/**
 * @brief Deletes a cli command
 *
 * @param [in] cli   pointer to cli instance
 * @param [in] name  command name
 */
void cli_delete_command(Cli* cli, const char* name);

/**
 * @brief Detects if Ctrl+C has been pressed or session has been terminated
 * 
 * @param [in] side Pointer to pipe side given to the command thread
 * @warning This function also assumes that the pipe is installed as the
 *          thread's stdio
 * @warning This function will consume 0 or 1 bytes from the pipe
 */
bool cli_app_should_stop(FuriPipeSide* side);

/** Print unified cmd usage tip
 *
 * @param      cmd    cmd name
 * @param      usage  usage tip
 * @param      arg    arg passed by user
 */
void cli_print_usage(const char* cmd, const char* usage, const char* arg);

#ifdef __cplusplus
}
#endif
