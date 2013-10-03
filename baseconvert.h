int baseconvert_is_valid_in_base(unsigned char base, char* digits, char* chars, unsigned int len);

void baseconvert_group_size(unsigned int in_len, unsigned int out_len, unsigned char in_base, unsigned char out_base, unsigned char* in_group_size, unsigned char* out_group_size);

unsigned int baseconvert_targetlen(unsigned char in_base, unsigned char out_base, unsigned int in_len);

unsigned int baseconvert(unsigned char in_base, char* in_digits, char* in_chars, unsigned int in_len, unsigned char out_base, char* out_digits, char* out_chars, unsigned int out_len );

