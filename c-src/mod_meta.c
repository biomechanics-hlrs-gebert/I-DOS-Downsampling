/**
* \file
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
* \brief Implementation file for the meta file format
*/

//include statements
#include "mod_meta.h"


// ---- DECLARATIONS ----
static inline void __meta_get_keyword_occurence_behaviour(KeywordBehaviour[2]);
static signed int __meta_get_keyword_index(int, int);
static ssize_t __meta_get_filesize(char *);
static char *__meta_get_file_suffix(char *);
static void __meta_zero_basename_struct(basename *);
static int __meta_fill_basename_struct(basename *);


// ==== IMPLEMENTATIONS ====
// ---- public function implementations ----
/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method for parsing a character parameter from a keyword.
*
* \param[in] fh A filehandle to a opened metafile.
* \param[in] keyword The keyword to parse.
* \param[in] metafile A pointer to a metafile structure.
* \param[out] value Array where the value may be stored. Must be large enough.
* \return 0 on success, 1 otherwise.
*/
int meta_read_string(FILE *fh, char *keyword, metafile *metafile, char *value){
    char additional_kwargs[META_MCL], value_internal[DIMENSIONS][META_MCL];
    
    if(meta_extract_keyword_data(fh, keyword, 0, metafile, value_internal, additional_kwargs)) return 1;
    strcpy(value, value_internal[0]);

    return 0;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method for parsing a integer parameter from a keyword.
*
* \param[in] fh A filehandle to a opened metafile.
* \param[in] keyword The keyword to parse.
* \param[in] metafile A pointer to a metafile structure.
* \param[out] value Pointer to integer where the value may be stored.
* \return 0 on success, 1 otherwise.
*/
int meta_read_int_0D(FILE *fh, char *keyword, metafile *metafile, int *value){
    char additional_kwargs[META_MCL], value_internal[DIMENSIONS][META_MCL];
    
    if(meta_extract_keyword_data(fh, keyword, 0, metafile, value_internal, additional_kwargs)) return 1;
    *value = atoi(value_internal[0]);

    return 0;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method for parsing an array of integer parameters from a keyword.
*
* \param[in] fh A filehandle to a opened metafile.
* \param[in] keyword The keyword to parse.
* \param[in] metafile A pointer to a metafile structure.
* \param[out] values An array where the integers may be stored.
* \return 0 on success, 1 otherwise.
*/
int meta_read_int_1D(FILE *fh, char *keyword, metafile *metafile, int values[DIMENSIONS]){
    char additional_kwargs[META_MCL], values_internal[DIMENSIONS][META_MCL];

    if(meta_extract_keyword_data(fh, keyword, 1, metafile, values_internal, additional_kwargs)) return 1;
    for(int i = 0; i < DIMENSIONS; i++)
        values[i] = atoi(values_internal[i]);
    return 0;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method for parsing a floating point parameter from a keyword.
*
* \param[in] fh A filehandle to a opened metafile.
* \param[in] keyword The keyword to parse.
* \param[in] metafile A pointer to a metafile structure.
* \param[out] value Pointer to a float where the value may be stored.
* \return 0 on success, 1 otherwise.
*/
int meta_read_double_0D(FILE *fh, char *keyword, metafile *metafile, double *value){
    char additional_kwargs[META_MCL], value_internal[DIMENSIONS][META_MCL];
    
    if(meta_extract_keyword_data(fh, keyword, 0, metafile, value_internal, additional_kwargs)) return 1;
    *value = atof(value_internal[0]);

    return 0;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method for parsing an array on array of floating point parameters from a keyword.
*
* \param[in] fh A filehandle to a opened metafile.
* \param[in] keyword The keyword to parse.
* \param[in] metafile A pointer to a metafile structure.
* \param[out] values Array where the values may be stored.
* \return 0 on success, 1 otherwise.
*/
int meta_read_double_1D(FILE *fh, char *keyword, metafile *metafile, double values[DIMENSIONS]){
    char additional_kwargs[META_MCL], values_internal[DIMENSIONS][META_MCL];
    
    if(meta_extract_keyword_data(fh, keyword, 1, metafile, values_internal, additional_kwargs)) 
        return 1;
    for(int i = 0; i < DIMENSIONS; i++)
        values[i] = atof(values_internal[i]);
    return 0;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method for writing a keyword with corresponding integer scalar value.
*
* \param[in] fh File handle to a metafile-file.
* \param[in] keyword Keyword to write to the metafile.
* \param[in] unit Unit to write behind the data. Nullable.
* \parma[in] log_date_flag Flag if the date should be logged behind the unit.
* \param[in] value The integer scalar value to be written behind the keyword.
* \return 0 on success, 1 otherwise.
*/
int meta_write_int_0D(FILE *fh, char *keyword, char *unit, int log_date_flag, int value){
    char buffer[META_PARAMETER_LENGTH];

    if(snprintf(buffer, META_PARAMETER_LENGTH, "%d", value) == META_PARAMETER_LENGTH) 
        return 1;
    return meta_write_keyword(fh, keyword, buffer, unit, log_date_flag);
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method for writing a keyword with corresponding integer vector value.
*
* \param[in] fh File handle to a metafile-file.
* \param[in] keyword Keyword to write to the metafile.
* \param[in] unit Unit to write behind the data. Nullable.
* \parma[in] log_date_flag Flag if the date should be logged behind the unit.
* \param[in] value The integer vector value to be written behind the keyword.
* \return 0 on success, 1 otherwise.
*/
int meta_write_int_1D(FILE *fh, char *keyword, char *unit, int log_date_flag, int value[DIMENSIONS]){
    if(value == NULL) return 1;

    char buffer[META_PARAMETER_LENGTH], tmp[META_PARAMETER_LENGTH];
    size_t string_length = 0;
    buffer[0] = '\0';
    for(int i = 0; i < DIMENSIONS; i++){
        if((string_length += snprintf(tmp, META_PARAMETER_LENGTH, "%d", value[i])) >= META_PARAMETER_LENGTH) return 1;
        strcat(buffer, tmp);
    }
    return meta_write_keyword(fh, keyword, buffer, unit, log_date_flag);
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method for writing a keyword with corresponding long scalar value.
*
* \param[in] fh File handle to a metafile-file.
* \param[in] keyword Keyword to write to the metafile.
* \param[in] unit Unit to write behind the data. Nullable.
* \parma[in] log_date_flag Flag if the date should be logged behind the unit.
* \param[in] value The long scalar value to be written behind the keyword.
* \return 0 on success, 1 otherwise.
*/
int meta_write_long_0D(FILE *fh, char *keyword, char *unit, int log_date_flag, long long value){
    char buffer[META_PARAMETER_LENGTH];
    if(snprintf(buffer, META_PARAMETER_LENGTH, "%lld", value) == META_PARAMETER_LENGTH) return 1;
    return meta_write_keyword(fh, keyword, buffer, unit, log_date_flag);
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method for writing a keyword with corresponding long vector value.
*
* \param[in] fh File handle to a metafile-file.
* \param[in] keyword Keyword to write to the metafile.
* \param[in] unit Unit to write behind the data. Nullable.
* \parma[in] log_date_flag Flag if the date should be logged behind the unit.
* \param[in] value The long vector value to be written behind the keyword.
* \return 0 on success, 1 otherwise.
*/
int meta_write_long_1D(FILE *fh, char *keyword, char *unit, int log_date_flag, long long value[DIMENSIONS]){
    if(value == NULL) return 1;

    char buffer[META_PARAMETER_LENGTH], tmp[META_PARAMETER_LENGTH];
    size_t string_length = 0;
    buffer[0] = '\0';
    for(int i = 0; i < DIMENSIONS; i++){
        if((string_length += snprintf(tmp, META_PARAMETER_LENGTH, "%lld", value[i])) >= META_PARAMETER_LENGTH) return 1;
        strcat(buffer, tmp);
    }
    return meta_write_keyword(fh, keyword, buffer, unit, log_date_flag);
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method for writing a keyword with corresponding double scalar value.
*
* \param[in] fh File handle to a metafile-file.
* \param[in] keyword Keyword to write to the metafile.
* \param[in] unit Unit to write behind the data. Nullable.
* \parma[in] log_date_flag Flag if the date should be logged behind the unit.
* \param[in] value The double scalar value to be written behind the keyword.
* \return 0 on success, 1 otherwise.
*/
int meta_write_double_0D(FILE *fh, char *keyword, char *unit, int log_date_flag, double value){
    char buffer[META_PARAMETER_LENGTH];
    if(snprintf(buffer, META_PARAMETER_LENGTH, "%lf", value) == META_PARAMETER_LENGTH) return 1;
    return meta_write_keyword(fh, keyword, buffer, unit, log_date_flag);
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method for writing a keyword with corresponding double vector value.
*
* \param[in] fh File handle to a metafile-file.
* \param[in] keyword Keyword to write to the metafile.
* \param[in] unit Unit to write behind the data. Nullable.
* \parma[in] log_date_flag Flag if the date should be logged behind the unit.
* \param[in] value The double vector value to be written behind the keyword.
* \return 0 on success, 1 otherwise.
*/
int meta_write_double_1D(FILE *fh, char *keyword, char *unit, int log_date_flag, double value[DIMENSIONS]){
    if(value == NULL) return 1;
    char buffer[META_PARAMETER_LENGTH], tmp[META_PARAMETER_LENGTH];
    size_t string_length = 0;
    buffer[0] = '\0';
    for(int i = 0; i < DIMENSIONS; i++){
        if((string_length += snprintf(tmp, META_PARAMETER_LENGTH, "%lf", value[i])) >= META_PARAMETER_LENGTH) return 1;
        strcat(buffer, tmp);
    }
    return meta_write_keyword(fh, keyword, buffe
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method for writing a keyword with corresponding string value.
*
* \param[in] fh File handle to a metafile-file.
* \param[in] keyword Keyword to write to the metafile.
* \param[in] unit Unit to write behind the data. Nullable.
* \parma[in] log_date_flag Flag if the date should be logged behind the unit.
* \param[in] value The string value to be written behind the keyword.
* \return 0 on success, 1 otherwise.
*/
int meta_write_string(FILE *fh, char *keyword, char *unit, int log_date_flag, char *value){
    if(value == NULL) return 1;
    if(strnlen(value, META_PARAMETER_LENGTH) == META_PARAMETER_LENGTH) return 1;
    return meta_write_keyword(fh, keyword, value, unit, log_date_flag);
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method for handling the locking file to control repeated program execution.
*
* \param[inout] restart Wether to restart or not to.
* \param[in] restart_cmdarg Zeroable. Possible cmd argument override.
* \return 0 on success, 1 otherwise.
*/
int meta_handle_lock_file(char *restart, char restart_cmdarg){
    int exist = 0;
    long long ios;
    char lockname[META_MCL];
    
    if(restart_cmdarg){
        if(restart_cmdarg != ''){
            //TODO
        }
    }
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Module to extract the data string of keywords
*
* Description:
*   Module to parse information of keywords.
*   An arbitrary keyword with up to META_MCL characters may be specified.
*
* \param[in] fh File handle to a opened metafile.
* \param[in] keyword String that holds the keyword to be searched.
* \param[in] dimensions Integer specifing if you want a moredimensional value (dimensions=1) or a scalar(dimensions=0). Do not confuse with DIMENSIONS macro.
* \param[in] metafile Pointer to a metafile structure to parse the keywords from.
* \param[out] keyword_arguments If(dimenisons == 1) each value in this array will be filled with the corresponding entry in the metafile, else [0] will be filled with the corrensonding scalar.
* \param[out] additional_keyword_argument An array holding all additional content (timestamps, units) as one string. Parse yourself into smaller parts.
* \return 0 on success, 1 otherwise
*/
int meta_extract_keyword_data(FILE *fh, char *keyword, int dimensions, metafile *metafile, char keyword_arguments[DIMENSIONS][META_MCL], char additional_keyword_argument[META_MCL]){
    if(fh == NULL || keyword == NULL || metafile == NULL || keyword_arguments == NULL || additional_keywird_arguments == NULL || /*global*/ global_meta_program_keyword == NULL) return 1;
    if(strnlen(keyword, META_KEYWORD_MAX_LENGTH) == META_KEYWORD_MAX_LENGTH) return 1;

    signed int keyword_found = -1, own_section = 0, own_keyword_index = -1, chosen_keyword_index = -1;
    char buffer[META_MCL], additional_buffer[META_MCL], kwargs_storage[META_MAX_NUMBER_OF_KEYWORD_ITERATIONS][DIMENSIONS][META_PARAMETER_MAX_LENGTH], additional_kwargs_storage[META_MAX_NUMBER_OF_KEYWORD_ITERATIONS][META_MCL];
    char *token, *newline_char, *comment_occurrence;
    
    //zero the keyword storage
    __meta_zero_array(META_MAX_NUMBER_OF_KEYWORD_ITERATIONS * DIMENSIONS * META_PARAMETER_MAX_LENGTH, (void *) kwargs_storage);
    __meta_zero_array(META_MAX_NUMBER_OF_KEYWORD_ITERATIONS * META_MCL, (void *) additional_kwargs_storage);

    //parse the metafile line by line
    for(int i = 0; i < metafile -> number_of_lines; i++){
        if(__meta_get_metafile_string_copy(metafile, i, buffer)) return 1; //Error: there was some problem while trying to copy the metafile line
        token = strtok(buffer, META_KEYWORD_SEPARATOR);

        if(token == NULL) return 1; //Error: something went wrong and there is a line without a string content in it 
        if(token[0] == '\n' || token[0] == '\0') continue; //skip the newline / empty string
        if(!strcmp(token, META_COMMENT_DECLARATOR)) continue; //skip the comment
        if(!strcmp(token, META_SECTION_DECLARATOR)){ //handle new section
            token = strtok(NULL, META_KEYWORD_SEPARATOR);
            if((newline_char = strchr(token, '\n')) != NULL) *newline_char = '\0';
            if(!strcmp(token, /*global*/ global_meta_program_keyword)) own_section = 1;
            else own_section = 0;
            continue;
        }
        else if(!strcmp(token, META_KEYWORD_DECLARATOR)){ //handle new keyword
            token = strtok(NULL, META_KEYWORD_SEPARATOR);
            if(token == NULL) return 1; //Error: no keyword specified
            if(!strcmp(token, keyword)){
                if(strnlen(token, META_KEYWORD_MAX_LENGTH) == META_KEYWORD_MAX_LENGTH) return 1; //Keyword too long
                token = strtok(NULL, META_KEYWORD_SEPARATOR);
                if(token == NULL) return 1; //Error: keyword does not have any value assigned to it.
                keyword_found++;
                if(dimensions == 0){
                    if(strnlen(token, META_PARAMETER_MAX_LENGTH) == META_PARAMETER_MAX_LENGTH) return 1; //Keyword too long
                    strcpy(kwargs_storage[keyword_found][0], token);
                }
                else if(dimensions == 1){
                    for(int j = 0; j < DIMENSIONS - 1; j++){
                        if(token == NULL) return 1; //error: dimensions=1 was given but there are not enough values to describe all DIMENSIONS
                        if(strnlen(token, META_PARAMETER_MAX_LENGTH) == META_PARAMETER_MAX_LENGTH) return 1; //Keyword too long
                        strcpy(kwargs_storage[keyword_found][j], token);
                        token = strtok(NULL, META_KEYWORD_SEPARATOR);
                    }
                    if(token == NULL) return 1; //error: dimensions=1 was given but there are not enough values to describe all DIMENSIONS
                    if(strnlen(token, META_PARAMETER_MAX_LENGTH) == META_PARAMETER_MAX_LENGTH) return 1; //Keyword too long
                    strcpy(kwargs_storage[keyword_found][DIMENSIONS - 1], token);
                }
                else return 1; //Error: incorrect dimension of dimension vector defined
                if(own_section) own_keyword_index = keyword_found;
                additional_buffer[0] = '\0';
                token = strtok(NULL, META_KEYWORD_SEPARATOR);
                for(int j = 0; token != NULL; j++){
                    comment_occurrence = strstr(token, META_COMMENT_DECLARATOR);
                    if(comment_occurrence != NULL) break;
                    if(j) strcat(additional_buffer, META_KEYWORD_SEPARATOR);
                    strcat(additional_buffer, token);
                    token = strtok(NULL, META_KEYWORD_SEPARATOR);
                }
                strcpy(additional_kwargs_storage[keyword_found], additional_buffer);
            }
        }
        else return 1; //Error: line cannot be interpreted, therefore exit
    }

    //check error cases
    if(keyword_found < 0) return 1; //the whole file does not contain the required keyword.

    //from the parsed possible keyword arguments, get the one with preference set at compile time.
    chosen_keyword_index = __meta_get_keyword_index(own_keyword_index, keyword_found);
    
    //remove newlines from all buffers and return them.
    if(dimensions == 0){
        newline_char = strchr(kwargs_storage[chosen_keyword_index][0], '\n');
        if(newline_char != NULL) *newline_char = '\0';
        strcpy(keyword_arguments[0], kwargs_storage[chosen_keyword_index][0]);
        newline_char = strchr(additional_kwargs_storage[chosen_keyword_index], '\n');
        if(newline_char != NULL) *newline_char = '\0';
        strcpy(additional_keyword_argument, additional_kwargs_storage[chosen_keyword_index]);
    }
    else if(dimensions == 1){
        for(int i = 0; i < DIMENSIONS; i++){
            newline_char = strchr(kwargs_storage[chosen_keyword_index][i], '\n');
            if(newline_char != NULL) *newline_char = '\0';
            strcpy(keyword_arguments[i], kwargs_storage[chosen_keyword_index][i]);
        }
        newline_char = strchr(additional_kwargs_storage[chosen_keyword_index], '\n');
        if(newline_char != NULL) *newline_char = '\0';
        strcpy(additional_keyword_argument, additional_kwargs_storage[chosen_keyword_index]);
    }
    else return 1; //dimension error
    return 0;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method for writing a keyword and its corresponding values to the metafile.
*
* \param[in] fh File handle to a metafile-file.
* \param[in] keyword Keyword to write to the metafile.
* \param[in] data Data to write behind the keyword, formatted as string.
* \param[in] unit Unit to write behind the data. Nullable.
* \parma[in] log_date_flag Flag if the date should be logged behind the unit.
* \return 0 on success, 1 otherwise.
*/
int meta_write_keyword(FILE *fh, char *keyword, char *data, char *unit, int log_date_flag){
    
    if(fh == NULL || keyword == NULL || data == NULL) return 1;

    char metafile_line[META_MCL];
    time_t now_time;
    struct tm *time_struct;
    size_t keyword_separator_size, extra_size = META_EXTRA_LENGTH, string_size;

    keyword_separator_size = strnlen(META_KEYWORD_SEPARATOR, META_MCL);

    if(strnlen(keyword, META_KEYWORD_MAX_LENGTH) == META_KEYWORD_MAX_LENGTH) return 1;
    if(strnlen(data, META_PARAMETER_MAX_LENGTH) == META_PARAMETER_MAX_LENGTH) return 1;
    extra_size -= 3 * keyword_separator_size;
    if(unit != NULL){
        if(strnlen(unit, META_UNIT_MAX_LENGTH) + keyword_separator_size >= META_UNIT_MAX_LENGTH) return 1;
        extra_size -= keyword_separator_size;
    }
    char time_buffer[extra_size];
    if(log_date_flag){
        now_time = time(NULL);
        time_struct = localtime(&now_time);
        if(strftime(time_buffer, sizeof(time_buffer), "%c", time_struct) >= extra_size) return 1;
    }
    
    //fill the metafile line
    strcpy(metafile_line, META_KEYWORD_DECLARATOR);
    strcat(metafile_line, META_KEYWORD_SEPARATOR);
    strcat(metafile_line, keyword);
    strcat(metafile_line, META_KEYWORD_SEPARATOR);
    strcat(metafile_line, data);
    if(unit != NULL){
        strcat(metafile_line, META_KEYWORD_SEPARATOR);
        strcat(metafile_line, unit);
    }
    if(log_date_flag){
        strcat(metafile_line, META_KEYWORD_SEPARATOR);
        strcat(metafile_line, time_buffer);
    }
    strcat(metafile_line, "\n");
    string_size = strnlen(metafile_line, META_MCL);
    //write to the metafile
    if(string_size != fprintf(fh, metafile_line)) return 1;
    return 0;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
* 
* \brief Subroutine to open a meta file to append data / keywords.
* 
* \param[out] metafile A pointer to a metafile structure.
* \return an errorcode.
*/
int meta_append(metafile *metafile){
    if(meta_invoke(metafile)) return 1;
    if(meta_continue(metafile)) return 1;
    if(meta_write(fhmeo, "META_PARSED/INVOKED", "Now - Date/Time on the right."))
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method for creating a new meta file.
*
* \param[in] filename_with_suffix The filename for the new metafile.
* \return 0 on success, 1 otherwise.
*/
int meta_create_new(char *filename_with_suffix){
    if(filename_with_suffix == NULL) return 1;

    size_t filename_length = strlen(filename_with_suffix);
    char buffer[filename_length + strlen(META_SUFFIX)];
    FILE *fh;

    if(__meta_get_filesize(filename_with_suffix) == -1) return 1; //file does not exist
    if(meta_parse_basename(filename_with_suffix, __meta_get_file_suffix(filename_with_suffix))) return 1;
    strcpy(buffer, /*global*/ in -> path_and_basename);
    strcat(buffer, META_SUFFIX);
    if(__meta_get_filesize(buffer) != -1) return 1; //metafile already exists

    /*global*/ fh_meta_put = fopen(buffer, "w+");
    if(/*global*/ fh_meta_put == NULL) return 1;
    return 0;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method to open and prepare a meta file for use.
*
* \param[inout] metafile Pointer to a metafile structure.
* \return 0 on success, 1 otherwise.
*/
int meta_invoke(metafile *metafile){
    long long lines;
    if(metafile == NULL) return 1;
    if(__meta_get_filesize(/*global*/ in -> full_name) == -1) return 1; //File not found
    int buffer_size;
    char *meta_suffix = __meta_get_file_suffix(/*global*/ in -> full_name);
    char filename_buffer[strlen(meta_suffix) + strlen(META_SUFFIX_DECLARATOR) + 1], *line_buffer;
    strcpy(filename_buffer, META_SUFFIX_DECLARATOR);
    strcat(filename_buffer, meta_suffix);
    if(strcmp(filename_buffer, META_SUFFIX) != 0) return 1; //Error: not a metafile
    if(meta_parse_basename(/*global*/ in -> full_name, META_SUFFIX)) return 1;
    /*global*/ fh_meta_in = fopen(/*global*/ in -> full_name, "r+");
    lines = meta_count_lines(/*global*/ fh_meta_in);
    if(lines == -1) return 1;
    for(int i = 0; i < lines; i++){
        if(getline(line_buffer, &buffer_size, /*global*/ fh_meta_in) == -1){
            free(line_buffer);
            return 1; //internal getline error
        }
        if(buffer_size >= META_MCL){
            free(line_buffer);
            return 1; //line too long
        }
        if(meta_set_metafile_string(metafile, i, line_buffer)){
            free(line_buffer);
            return 1; //setting of string doesn't work
        }
        
    }
    free(line_buffer);
    return 0;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method to invoke the meta output file.
*
* \param[inout] metafile Pointer to a metafile structure.
* \return 0 on success, 1 otherwise.
*/
/*
* TODO: Implement length checking for out -> basename! May be errors incoming on the length of this.
* TODO: File pointer passing around is very bad, as it is very unclear what access modes are used, what else is going on with the file
*/
int meta_continue(metafile *){
    if(metafile == NULL) return 1;

    if(meta_read_string(/*global*/ fh_meta_put, "NEW_BSNM_FEATURE", metafile, /*global*/ out -> features)) return 1;
    if(meta_read_string(/*global*/ fh_meta_put, "NEW_BSNM_PURPOSE", metafile, /*global*/ out -> purpose)) return 1;

    if(!strcmp(/*global*/ out -> purpose, /*global*/ in -> purpose) && !strcmp(/*global*/ out -> features, /*global*/ in -> features)){
        /*pass, TODO*/
        /*echo WARNING: BASENAME DID NOT CHANGE (in part)*/
    }
    strcpy(/*global*/ out -> basename, /*global*/ out -> dataset);
    strcat(/*global*/ out -> basename, META_BASENAME_SEPARATOR);
    strcat(/*global*/ out -> basename, /*global*/ out -> type);
    strcat(/*global*/ out -> basename, META_BASENAME_SEPARATOR);
    strcat(/*global*/ out -> basename, /*global*/ out -> purpose);
    strcat(/*global*/ out -> basename, META_BASENAME_SEPARATOR);
    strcat(/*global*/ out -> basename, /*global*/ out -> purpose);
    strcat(/*global*/ out -> basename, META_BASENAME_SEPARATOR);
    strcat(/*global*/ out -> basename, /*global*/ global_meta_prgrm_mstr_app);
    strcat(/*global*/ out -> basename, META_BASENAME_SEPARATOR);
    strcat(/*global*/ out -> basename, /*global*/ out -> features);

    strcpy(/*global*/ out -> path_and_basename, /*global*/ out -> path);
    strcat(/*global*/ out -> path_and_basename, /*global*/ out -> basename);

    strcpy(/*global*/ out -> full_name, /*global*/ out -> path_and_basename);
    strcat(/*global*/ out -> path_and_basename, META_SUFFIX);

    char command[strlen(/*global*/ in -> full_name) + strlen(/*global*/ out -> full_name) + 5];
    ssize_t beginning_offset;
    strcpy(command, "cp ");
    strcat(command, /*global*/ in -> full_name);
    strcat(command, " ");
    strcat(command, /*global*/ out -> full_name);
    if(system(command)) return 1; //Error: copy of file failed
    
    if(/*global*/ fh_meta_out == NULL) return 1; //file pointer not filled
    beginning_offset = ftell(/*global*/ fh_meta_out);
    if(beginning_offset < 0) return 1; //error on position retrieval occurred
    if(fseek(/*global*/ fh_meta_put, SEEK_END)) return 1; //Error. some problems with seeking the file
    return 0;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method to deal with the logging and renaming of the log in context 
*        of the meta data approach.
*
* Description:
*   This subroutine only gets called after meta append or meta close respectively.
*   If the variable in/out are not set, the program will not start / stop accordingly.
*   During computation (before meta_stop_ascii), the files are called 'temporary.suffix'
*   to show which ones are subject to modifications.
*    
* \param[out] fh File handle pointer to be filled with temporary file.
* \param[in] suffix String holding the desired suffix of the logfile.
* \return 0 on success, 1 otherwise.
*/
int meta_start_ascii(FILE **fh, char *suffix){
    if(fh == NULL || suffix == NULL) return 1;
    
    size_t temp_size = strlen(/*global*/ out -> path) + strlen(suffix) + 10;
    size_t perm_size = strlen(/*global*/ out -> path_and_basename) + strlen(suffix) + 1;
    char temporary_filename[temp_size];
    char permanent_filename[perm_size];
    char temp_command[temp_size + 6];
    char perm_command[perm_size + 6];

    strcpy(temporary_filename, /*global*/ out -> path);
    strcat(temporary_filename, "temporary");
    strcat(temporary_filename, suffix);

    strcpy(permanent_filename, /*global*/ out -> path_and_basename);
    strcat(permanent_filename, suffix);

    if(__meta_get_filesize(temporary_filename) != -1){
        strcpy(temp_command, "rm -r ");
        strcat(temp_command, temporary_filename);
        if(system(temp_command)) return 1; //removal of temp file failed
    }

    if(__meta_get_filesize(permanent_filename) != -1){
        strcpy(perm_command, "rm -r ");
        strcat(perm_command, temporary_filename);
        if(system(perm_command)) return 1; //removal of perm file failed
    }

    *fh = fopen(temporary_filename, "w");
    if(*fh == NULL) return 1;
    return 0;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method to stop the logging and renaming of additional ascii files.
*
* \param[in] fh File handle to the input file.
* \param[in] suffix Suffix of the file.
* \return 0 on success, 1 otherwise.
*/
int meta_stop_ascii(FILE *fh, char *suffix){
    if(fh == NULL || suffix == NULL) return 1;
    
    size_t temp_size = strlen(/*global*/ out -> path) + strlen(suffix) + 10;
    size_t perm_size = strlen(/*global*/ out -> path_and_basename) + strlen(suffix) + 1;
    size_t alt_size = strlen(/*global*/ in -> path_and_basename) + strlen(suffix) + 1;
    char temporary_filename[temp_size];
    char permanent_filename[perm_size];
    char alternative_filename[alt_size];
    char exist_command[temp_size + perm_size + 5];
    char not_exist_command[alt_size + perm_size + 5];

    strcpy(temporary_filename, /*global*/ out -> path);
    strcat(temporary_filename, "temporary");
    strcat(temporary_filename, suffix);

    strcpy(permanent_filename, /*global*/ out -> path_and_basename);
    strcat(permanent_filename, suffix);

    strcpy(permanent_filename, /*global*/ in -> path_and_basename);
    strcat(permanent_filename, suffix);

    if(fclose(fh) == EOF) return 1;

    if(__meta_get_filesize(temporary_filename) != -1){
        strcpy(exist_command, "mv ");
        strcat(exist_command, temporary_filename);
        strcat(exist_command, " ");
        strcat(exist_command, permanent_filename);
        if(system(exist_command)) return 1;
    }
    else{
        if(__meta_get_filesize(alternative_filename) != -1){
            strcpy(not_exist_command, "cp ");
            strcat(not_exist_command, alternative_filename);
            strcat(not_exist_command, " ");
            strcat(not_exist_command, permanent_filename);
        }
        if(system(not_exist_command)) return 1;
    }
    return 0;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method to check and open ascii files wich must exist.
*
* Description:
*   For example to read input data. To stop the file, use meta_stop_ascii.
*
* \param[in] fh File handle to the input file.
* \param[in] suffix Suffix of the file.
* \param[out] amount_of_lines Interger pointer to store the number of lines in file.
* \return 0 on success, 1 otherwise.
*/
int meta_existing_ascii(FILE **fh, char *suffix, int *amount_of_lines){
    if(fh == NULL || suffix == NULL || amount_of_lines == NULL) return 1;

    size_t filename_size = strlen(/*global*/ in -> path_and_basename) + strlen(suffix) + 1;
    char filename[filename_size];
    
    strcpy(filename, /*global*/ in -> path_and_basename);
    strcat(filename, suffix);

    if(__meta_get_filesize(filename) == -1) return 1;
    *fh = fopen(filename, "r+");
    if(*fh == NULL) return 1;
    *amount_of_lines = meta_count_lines(*fh);
    if(*amount_of_lines == -1) return 1;
    return 0;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method to sign a meta file at the end of computation.
*
* \param[in] binary The file of the to be hashed 
* \return 0 on success, 1 otherwise.
*/
int meta_signing(char *binary){
    if(binary == NULL) 
        return 1;

    //TODO implement missing parts
    return 0;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method to close a meta file.
*
* \param[in] size_mpi The number of processors used in computation. For serial, use 1.
* \return 0 on success, 1 otherwise.
*/
int meta_close(long long int size_mpi){
    
    if(meta_write_long_0D(fhmeo, "PROCESSORS", NULL, 0, size_mpi))
        return 1;
    
    //TODO timing missing

    return 0;
}

/**
*
*
*/
size_t meta_get_filesize(basename *);

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method to get the number of lines in a file.
*
* \warning OPEN THE FILE IN READ/READWRITE MODE TO USE THIS FUNCTION.
* \warning THIS HAS TO BE A TEXT FILE, OTHERWISE THE RESULTS WILL MAKE NO SENSE.
*
* \param[in] fh File handle to operate on.
* \return -1 if the file contains no newlines, else the number of lines.
*/
size_t meta_count_lines(FILE *fh){
    char ch, touched = 0;
    size_t number_of_lines = 0, i = 0;
    
    if(fh == NULL) return -1;
    while((i++ < META_MAX_FILE_LINES * META_MCL) && ((ch = fgetc(fh)) != EOF))
        if(ch == '\n'){
            number_of_lines++;
            touched = 1;
        }
    if(fseek(fh, 1 - i, SEEK_CUR)) return -1;
    return number_of_lines * touched - (touched - 1);
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method to parse the basename.
*
* \param[in] filename Full name of the file.
* \param[in] suffix Expected suffix.
*/
//TODO use different macro for length of path
int meta_parse_basename(char *filename, char *suffix);
    if(filename == NULL || suffix == NULL) return 1;

    char *filename_part, *basename_end, buffer[META_MCL];
    size_t number_of_lines;

    if(strnlen(filename, META_MCL) == META_MCL) return 1;

    __meta_zero_basename_struct((void *) /*global*/ in);
    strcpy(/*global*/ in -> full_name, filename);
    
    filename_part = strtok(filename, "/");
    if(filename_part == NULL) return 1;
    
    while(filename_part != NULL){
        if(strstr(filename_part, META_SUFFIX) != NULL)
            break;
        else{
            strcat(strcat(/*global*/ in -> path, "/"), filename_part);
            filename_part = strtok(NULL, "/");
        }
    }
    if(filename_part == NULL) return 1;

    strcpy(/*global*/ in -> basename, filename_part);
    basename_end = strstr(/*global*/ in -> basename, META_SUFFIX);
    *basename_end = '\0';
    strcat(/*global*/ in -> path, "/");
    strcpy(/*global*/ in -> path_and_basename, /*global*/ in -> path);
    strcat(/*global*/ in -> path_and_basename, /*global*/ in -> basename);
    
    strcpy(buffer, /*global*/ in -> basename);
    filename_part = strtok(buffer, "_");
    if(filename_part == NULL) return 1;
    strcpy(/*global*/ in -> dataset, filename_part);
    filename_part = strtok(NULL, "_");
    if(filename_part == NULL) return 1;
    strcpy(/*global*/ in -> type, filename_part);
    filename_part = strtok(NULL, "_");
    if(filename_part == NULL) return 1;
    strcpy(/*global*/ in -> purpose, filename_part);
    filename_part = strtok(NULL, "_");
    if(filename_part == NULL) return 1;
    strcpy(/*global*/ in -> app, filename_part);
    filename_part = strtok(NULL, "_");
    if(filename_part == NULL) return 1;
    strcpy(/*global*/ in -> features, filename_part);
    return 1;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method to check and in case trunctate length of given unit.
*
* \param[in] fh File handle to metafile. (TODO: unnecessary)
* \param[in] unit The physical unit to be checked.
* \return 0 if ok, 1 if trunctated, on error -1.
*/
int meta_check_unit(FILE *fh, char *unit){
    if(unit == NULL || fh == NULL) return -1;
    if(strnlen(unit, META_UNIT_MAX_LENGTH) < META_UNIT_MAX_LENGTH) return 0;
    unit[META_KEYWORD_MAX_LENGTH - 1] = '\0';
    return 1;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method to check and in case trunctate length of given keyword.
*
* \param[in] fh File handle to metafile. (TODO: unnecessary)
* \param[in] keyword The keyword to be checked.
* \return 0 if ok, 1 if trunctated, on error -1.
*/
int meta_check_keyword(FILE *fh, char *keyword){
    if(keyword == NULL || fh == NULL) return -1;
    if(strnlen(keyword, META_KEYWORD_MAX_LENGTH) < META_KEYWORD_MAX_LENGTH) return 0;
    keyword[META_KEYWORD_MAX_LENGTH - 1] = '\0';
    return 1;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method to take the sha256 hashing function of a certain binary and write it to metafile.
*
* \param[in] binary_name The file to be hashed.
* \return 0 on success, 1 otherwise.
*/
/*
* UNIFY HEAD:             |  done
* UNIFY DOC:              |  fortran error
* FUNCTIONAL (THEORY):    |  done
* FUNCTIONAL (PRACTICE):  | 
* UNIT TESTING:           | 
* ADD  ERROR HANDLER:     | 
* ADD  INPUT SANITIZER:   |  done
* MAKE PRETTY:            |  done
*/
int meta_write_sha256sum(char *binary_name){
    if(binary_name == NULL) 
        return 1;

    const char keyword[] = "SHA256SUM_OF_BINARY";
    size_t hash_size = 258 * sizeof(char);
    size_t command_size = strlen(binary_name + 46) * sizeof(char);
    char hash[hash_size], command[command_size] = "sha256sum ";
    int error = 0;
    FILE *fh;

    if(__meta_get_filesize("temp_buffer") != -1)
        if(system("rm -r temp_buffer")) 
            return 1;

    if(
        system("which cut > /dev/null 2> /dev/null") ||
        system("which sha256sum > /dev/null 2> /dev/null")
    ) 
        return 1;
    
    fh = fopen("temp_buffer", "w");
    if(fh == NULL) 
        return 1;
    fclose(fh);
    
    strcat(command, binary_name);
    strcat(command, " | cut -d ' ' -f 1 >> 'temp_buffer'");

    if(!system(command)){
        fh = fopen("temp_buffer", "r");
        if(fh == NULL) 
            return 1;
        if(fgets(hash, hash_size, fh) == NULL) 
            error = 1;
        fclose(fh);
        
        if(!error)
            if(meta_write_string(fhmeo, keyword, NULL, 0, hash)) 
                error = 1;
    }
    
    if(__meta_get_filesize("temp_buffer") != -1)
        if(system("rm -r temp_buffer")) 
            return 1;

    return error;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief Method to check filesize and delete file if 0.
*
* \param[in] filename Name of the file.
* \return 0 if file was deleted, 1 if not. On error -1.
*/
/*
* UNIFY HEAD:             |  
* UNIFY DOC:              |  
* FUNCTIONAL (THEORY):    |  
* FUNCTIONAL (PRACTICE):  | 
* UNIT TESTING:           | 
* ADD  ERROR HANDLER:     | 
* ADD  INPUT SANITIZER:   |  
* MAKE PRETTY:            |  
*/
int meta_delete_empty_file(char *filename){
    if(filename == NULL) return -1;

    ssize_t filesize = __meta_get_filesize(filename);
    char command[strlen(filename) + 7];

    if(filesize == -1) return -1;
    if(filesize > 0) return 1;

    strcpy(command, "rm -r ");
    strcat(command, filename);
    if(system(command)) return -1;
    return 0;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief getter to obtain a safe copy of a metafile line (= a string).
*
* \param[in] metafile A pointer to a metafile structure.
* \param[in] line_number The line one wants to get a copy of.
* \param[out] copy The buffer the metafile line should be copied to.
* \return on success 0, else 1.
*/
int meta_get_metafile_string_copy(metafile *metafile, int line_number, char *copy){
    if(metafile == NULL || copy == NULL) return 1;
    if(line_number >= META_MAX_FILE_LINES) return 1;
    if(strnlen(metafile -> content + line_number * META_MCL, META_MCL) == META_MCL) return 1;

    strcpy(copy, metafile -> content + line_number * META_MCL);
    return 0;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief setter to update a line in a metafile structure.
*
* \param[in] metafile A pointer to a metafile structure.
* \param[in] line_number The line one wants to alter.
* \param[in] string The replacement string for the requested line.
* \return on success 0, 1 otherwise.
*/
int meta_set_metafile_string(metafile *metafile, int line_number, char *string){
    if(metafile == NULL || string == NULL) return 1;
    if(line_number >= META_MAX_FILE_LINES) return 1;
    if(strnlen(string, META_MCL) == META_MCL) return 1;

    strcpy(metafile -> content + line_number * META_MCL, string);
    return 0;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief getter to obtain a reference (raw pointer) to a metafile line (= a string).
*
* \param[in] metafile A pointer to a metafile structure.
* \param[in] line_number The line one wants to get a reference on.
* \return Pointer to the requested line if possible, else or on error NULL.
*/
char *meta_get_metafile_string_reference(metafile *metafile, int line_number){
    if(metafile == NULL) return NULL;
    if(line_number >= META_MAX_FILE_LINES) return NULL;
    return (metafile -> content) + line_number * META_MCL * sizeof(char);
}

// ---- private function implementations ----

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief small library iternal to set all elements in a basename structure to zero
*
* \private
*
* \param[out] basename A pointer to the basename struct.
*/
static void __meta_zero_basename_struct(basename *basename){
        basename -> full_name[0] = '\0';
        basename -> path_and_basename[0] = '\0';
        basename -> basename[0] = '\0';
        basename -> dataset[0] = '\0';
        basename -> type[0] = '\0';
        basename -> purpose[0] = '\0';
        basename -> app[0] = '\0';
        basename -> features[0] = '\0';
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief small library internal to zero all elements in an array.
*
* \private
*
* \param[in] array_size The size of the array in bytes.
* \param[in] array The array to zero.
*/
static void __meta_zero_array(size_t array_size, void *array){
    char *internal_array = (char *) array; 
    for(int i = 0; i < array_size; i++) 
        internal_array[i] = '\0';
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief small library internal to get the filesize and existence of a file.
* 
* \private
*
* \param[in] filename The filename to be checked.
* \return -1 if file does not exist, filesize in bytes otherwise.
*/
static ssize_t __meta_get_filesize(char *filename){
    if(filename == NULL) return -1;
    struct stat st;
    if(stat(filaname, &st) != 0) return -1; //file not found
    return st.st_size;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief small library internal to select the appropriate keyword and its arguments from the keyword_arguments_storage.
*
* \private
*
* \param[in] kwargs_storage The palce where all occurences of the keyword (or more precicely their arguments) have been stored.
* \param[in] own_section_index An index in kwags_storage that marks the entry which belongs to the own program. 
* \return -1 on error, otherwise the index of the chosen keyword_argument in the storage.
*/
static signed int __meta_get_keyword_index(int own_section_index, int last_filled_line){
    KeywordBehaviour behaviour[2];
    int valid_choice = 0, position = -1;
    
    __meta_get_keyword_occurence_behaviour(behaviour);
    for(int i = 0; i < 2; i++){
        if(behaviour[i] == UNDEFINED) return -1; //error: no correct behaviour was set.
        else if(behaviour[i] == OWN){
            if(own_section_index >= 0) 
                return own_section_index;
        }
        else if(behaviour[i] == BEFORE){
            if(own_section_index > 0)
                return own_section_index - 1;
        }
        else if(behaviour[i] == AFTER){
            if(own_section_index >= 0 && own_section_index < META_MAX_NUMBER_OF_KEYWORD_ITERATIONS)
                return own_section_index + 1;
        }
        else if(behaviour[i] == FIRST)
            return 0;
        else if(behaviour[i] == LAST){
            return last_filled_line;
        }
    }
    return -1;
} 

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief small library internal inline function to parse the META_KEYWORD_OCCURENCE_BEHAVIOUR macro for the appropriate behaviour.
*
* \private
*
* \param[out] behaviour Integer array of length two which is filled with the primary and secondary behaviour.
*/
static inline void __meta_get_keyword_occurence_behaviour(KeywordBehaviour behaviour[2]){
    char *token, buffer[14];

    strcpy(buffer, META_KEYWORD_OCCURRENCE_BEHAVIOUR);
    behaviour[0] = UNDEFINED;
    behaviour[1] = UNDEFINED;
    token = strtok(buffer, ",");
    if(token == NULL) return;
    if(!strcmp(token, "own")) behaviour[0] = OWN;
    else if(!strcmp(token, "before")) behaviour[0] = BEFORE; 
    else if(!strcmp(token, "after")) behaviour[0] = AFTER;
    else if(!strcmp(token, "first")) behaviour[0] = FIRST;
    else if(!strcmp(token, "last")) behaviour[0] = LAST;
    
    //is the secondary option given?
    token = strtok(NULL, ",");
    if(token == NULL) return;
    
    //second iteration
    if(!strcmp(token, "own")) behaviour[1] = OWN;
    else if(!strcmp(token, "before")) behaviour[1] = BEFORE;
    else if(!strcmp(token, "after")) behaviour[1] = AFTER;
    else if(!strcmp(token, "first")) behaviour[1] = FIRST;
    else if(!strcmp(token, "last")) behaviour[1] = LAST;
    return;
}

/**
* \author Jonathan Schäfer, hpcjscha@hlrs.de, HLRS/NUM
*
* \brief library internal to get the file extension (suffix).
*
* \private
*
* \param[in] filename The filename to be parsed.
* \return NULL on error, on success pointer to a copy of the file extension.
*/
static char *__meta_get_file_suffix(char *filename){
    if(filename == NULL) return NULL;

    size_t filename_length = strlen(filename_with_suffix);
    char old_token[filename_length + 1];
    token = strtok(filename_with_suffix, META_SUFFIX_DECLARATOR);
    if(token == NULL) return NULL;
    while(token != NULL){
        strcpy(old_token, token);
        token = strtok(NULL, META_SUFFIX_DECLARATOR);
    }
    return old_token;
}

