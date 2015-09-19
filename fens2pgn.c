/* fens2pgn - converts multiple FENs into single PGN file
 * Copyright (C) 2015 Paweł Zacharek
 * 
 * -----------------------------------------------------------------------
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 * -----------------------------------------------------------------------
 * 
 * date: 2015-09-18
 * compiling: gcc -std=gnu11 -o fens2pgn.elf fens2pgn.c
 */

#include <ctype.h>
#include <errno.h>
#include <getopt.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define VERSION "0.6.1"

/* to store the longest hypothetical piece placement field in FEN:
 * "1r1k1b1r/p1n1q1p1/1p1n1p1p/P1p1p1P1/1P1p1P1P/B1P1P1K1/1N1P1N1R/R1Q2B1b" */
#define MAX_PLACEMENT_LENGHT 70
#define MAX_FEN_LENGHT 87  // after adding " b KQkq - 100 120"

#define STORE_SPACE_SIZE 6  // stands for the longest " 100. "
#define STORE_MOVE_SIZE 7  // stands for the longest "Qa1xb2+"

#define STR(x) STR_2(x)
#define STR_2(x) # x

enum what_to_write_exit_codes {
	W_None, W_In_Line, W_In_Column, W_Both, W_Other,
	W_Pawn_None, W_Pawn_Capture, W_Pawn_Promotion, W_Pawn_Capture_Promotion,
	W_Discard_Fen
};

struct structure_field {
	char alphabetical;
	signed char numerical;
	char piece_before;
	char piece_after;
};

struct structure_instruction {
	char to_x;
	signed char to_y;
};

struct structure_piece {
	char alphabetical;
	signed char numerical;
	char type;
} store_piece;

// lists directions in which a bishop can move
const struct structure_instruction instructions_bishop[4] = {{1, 1}, {1, -1}, {-1, -1}, {-1, 1}};
// lists directions in which a king can move
const struct structure_instruction instructions_king[8] = {{1, 0}, {1, 1}, {0, 1}, {-1, 1}, {-1, 0}, {-1, -1}, {0, -1}, {1, -1}};
// lists directions in which a knight can move
const struct structure_instruction instructions_knight[8] = {{2, 1}, {1, 2}, {-1, 2}, {-2, 1}, {-2, -1}, {-1, -2}, {1, -2}, {2, -1}};
// lists directions in which a rook can move
const struct structure_instruction instructions_rook[4] = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}};
/* lists directions in which an attacking pawn can be found
 * even numbered fields {0, 2} in array stands for white pawn,
 * whereas the odd ones {1, 3} stands for black pawn */
const struct structure_instruction instructions_pawn[4] = {{-1, -1}, {1, -1}, {-1, 1}, {1, 1}};

const struct structure_suffixes {
	char st[3];
	char nd[3];
	char rd[3];
	char th[3];
} Suffixes = {"st", "nd", "rd", "th"};

// Determines if field (x, y) is inside chess board.
bool are_coords_valid(char x, signed char y)
{
	if (x >= 'a' && x <= 'h' && y >= 1 && y <= 8)
		return 1;
	return 0;
}

// Function writes up to 4 differences between boards to array 'distinctions'.
static inline signed char compare_boards(const char (*Board_1)[8], const char (*Board_2)[8], struct structure_field distinctions[])
{
	signed char differences = 0;
	for (signed char y = 0; y < 8; ++y)
		for (signed char x = 0; x < 8; ++x)
			if (Board_1[y][x] != Board_2[y][x]) {  // two boards differ in that field
				if (++differences > 4)  // checks if array 'distinctions' is too small to hold all differences
					continue;
				distinctions[differences - 1].alphabetical = 'a' + x;
				distinctions[differences - 1].numerical = 8 - y;
				distinctions[differences - 1].piece_before = Board_1[y][x];
				distinctions[differences - 1].piece_after = Board_2[y][x];
			}
	return differences;
}

// Reads FEN string and fill in the array representing chess board.
bool fen2board(char *fen, char (*board)[8], bool validate)
{
	memset(board, ' ', 8 * 8 * sizeof(char));
	if (validate != 1) {
		for (signed char y = 0; y < 8; ++y, ++fen)
			for (signed char x = 0; x < 8; ++fen) {
				if (*fen >= '1' && *fen <= '8')
					x += *fen - '0';
				else
					board[y][x++] = *fen;
			}
	} else {  // basic error checking (we support some chess variants)
		signed char white_kings = 0, black_kings = 0;
		for (signed char y = 0; y < 8; ++y, ++fen)
			for (signed char x = 0; x < 8; ++fen) {
				if (*fen >= '1' && *fen <= '8')
					x += *fen - '0';
				else if (toupper(*fen) == 'K') {
					isupper(*fen) ? ++white_kings : ++black_kings;
					board[y][x++] = *fen;
				} else if (toupper(*fen) == 'P' && y != 0 && y != 7)
					board[y][x++] = *fen;
				else if (toupper(*fen) == 'R' || toupper(*fen) == 'N' || toupper(*fen) == 'B' || toupper(*fen) == 'Q')
					board[y][x++] = *fen;
				else
					return 0;
			}
		if (white_kings != 1 || black_kings != 1)
			return 0;
	}
	return 1;
}

/* Writes to structure of type 'structure_piece' coordinates of the first found
 * piece of type 'type_of_piece'; useful in finding kings. */
bool find_piece(char type_of_piece, struct structure_piece *piece, const char (*Board)[8])
{
	for (signed char y = 0; y < 8; ++y)
		for (signed char x = 0; x < 8; ++x)
			if (Board[y][x] == type_of_piece) {
				piece->alphabetical = 'a' + x;
				piece->numerical = 8 - y;
				piece->type = type_of_piece;
				return 1;
			}
	return 0;
}

/* Returns the first piece encountered by simultaneously incrementing coordinates
 * by values 'to_x' and 'to_y'. If no piece has been found, the function returns
 * space sign. Used to determine if field is under attack */
char increment_and_return_encountered_piece(char x, signed char y, char to_x, signed char to_y, const char (*Board)[8])
{
	for (x += to_x, y += to_y; are_coords_valid(x, y); x += to_x, y += to_y)
		if (Board[8 - y][x - 'a'] != ' ') {
			store_piece.alphabetical = x;
			store_piece.numerical = y;
			store_piece.type = Board[8 - y][x - 'a'];
			return Board[8 - y][x - 'a'];
		}
	return ' ';
}

// Determines if piece is under attack.
bool is_piece_attacked(const char Piece, const char A, const signed char B, const char (*Board)[8], signed char which_attack)
{
	const char Base = (islower(Piece) ? '\0' : 'a' - 'A');
	char x, encountered_piece;
	signed char y, number_of_attacks = 0;
	if (which_attack < 0) {  // NOTE: it means that we don't want to attack field (A, B), but cover it
		which_attack = abs(which_attack);
		x = A;
		if (islower(Piece) && (are_coords_valid(A, B - 1) && Board[8 - (B - 1)][A - 'a'] == 'P' || B - 2 == 2 && Board[8 - (B - 1)][A - 'a'] == ' ' && Board[8 - (B - 2)][A - 'a'] == 'P')) {
			y = B + (Board[8 - (B - 1)][A - 'a'] == 'P' ? -1 : -2);
			if (++number_of_attacks == which_attack)
				goto attacker_has_been_found;
		} else if (isupper(Piece) && (are_coords_valid(A, B + 1) && Board[8 - (B + 1)][A - 'a'] == 'p' || B + 2 == 7 && Board[8 - (B + 1)][A - 'a'] == ' ' && Board[8 - (B + 2)][A - 'a'] == 'p')) {
			y = B + (Board[8 - (B + 1)][A - 'a'] == 'p' ? 1 : 2);
			if (++number_of_attacks == which_attack)
				goto attacker_has_been_found;
		}
	} else
		for (signed char i = (islower(Piece) ? 0 : 1); i < 4; i += 2) {  // is there any PAWN attacking piece?
			x = A + instructions_pawn[i].to_x;
			y = B + instructions_pawn[i].to_y;
			if (are_coords_valid(x, y) && Board[8 - y][x - 'a'] == Base + 'P')
				if (++number_of_attacks == which_attack)
					goto attacker_has_been_found;
		}
	for (signed char i = 0; i < 4; ++i) {  // is there any ROOK or QUEEN attacking piece?
		encountered_piece = increment_and_return_encountered_piece(A, B, instructions_rook[i].to_x, instructions_rook[i].to_y, Board);
		if (encountered_piece == Base + 'R' || encountered_piece == Base + 'Q')
			if (++number_of_attacks == which_attack)
				return 1;
	}
	for (signed char i = 0; i < 4; ++i) {  // is there any BISHOP or QUEEN attacking piece?
		encountered_piece = increment_and_return_encountered_piece(A, B, instructions_bishop[i].to_x, instructions_bishop[i].to_y, Board);
		if (encountered_piece == Base + 'B' || encountered_piece == Base + 'Q')
			if (++number_of_attacks == which_attack)
				return 1;
	}
	for (signed char i = 0; i < 8; ++i) {  // is there any KNIGHT attacking piece?
		x = A + instructions_knight[i].to_x;
		y = B + instructions_knight[i].to_y;
		if (are_coords_valid(x, y) && Board[8 - y][x - 'a'] == Base + 'N')
			if (++number_of_attacks == which_attack)
				goto attacker_has_been_found;
	}
	for (signed char i = 0; i < 8; ++i) {  // is there any KING attacking piece?
		x = A + instructions_king[i].to_x;
		y = B + instructions_king[i].to_y;
		if (are_coords_valid(x, y) && Board[8 - y][x - 'a'] == Base + 'K')
			if (++number_of_attacks == which_attack)
				goto attacker_has_been_found;
	}
	return 0;
attacker_has_been_found:
	store_piece.alphabetical = x;
	store_piece.numerical = y;
	store_piece.type = Board[8 - y][x - 'a'];
	return 1;
}

// We know that king is attacked in (A, B) and can't escape.
bool is_it_checkmate(const char A, const signed char B, const char (*Board)[8], const char En_Passant_Field[])
{
	const char King = Board[8 - B][A - 'a'];
	is_piece_attacked(King, A, B, Board, 1);
	const char Piece = store_piece.type, C = store_piece.alphabetical;
	const signed char D = store_piece.numerical;
	char (*new_board)[8] = (char (*)[8])malloc(8 * 8 * sizeof(char));
	memcpy(new_board, Board, 8 * 8 * sizeof(char));
	for (signed char i = 1; is_piece_attacked(Piece, C, D, Board, i); ++i) {  // can we capture the attacker?
		new_board[8 - store_piece.numerical][store_piece.alphabetical - 'a'] = ' ';
		new_board[8 - D][C - 'a'] = store_piece.type;
		if (is_piece_attacked(King, A, B, (const char (*)[8])new_board, 1) == 0)
			return 0;
		new_board[8 - D][C - 'a'] = Piece;
		new_board[8 - store_piece.numerical][store_piece.alphabetical - 'a'] = store_piece.type;
	}
	switch (toupper(Piece)) {
		case 'P':  // we'll try to capture the attacker en passant
		{
			if (En_Passant_Field[0] != C)  // we definitely can't capture the attacker en passant
				return 1;
			const char Defender = (islower(Piece) ? 'P' : 'p');
			if (C != 'a' && new_board[8 - D][C - 1 - 'a'] == Defender) {
				new_board[8 - D][C - 'a'] = ' ';
				new_board[8 - D][C - 1 - 'a'] = ' ';
				new_board[8 - (D + (islower(Piece) ? 1 : -1))][C - 'a'] = Defender;
				if (is_piece_attacked(King, A, B, (const char (*)[8])new_board, 1) == 0)
					return 0;
				new_board[8 - (D + (islower(Piece) ? 1 : -1))][C - 'a'] = ' ';
				new_board[8 - D][C - 1 - 'a'] = Defender;
				new_board[8 - D][C - 'a'] = Piece;
			}
			if (C != 'h' && new_board[8 - D][C - 'a' + 1] == Defender) {
				new_board[8 - D][C - 'a'] = ' ';
				new_board[8 - D][C - 'a' + 1] = ' ';
				new_board[8 - (D + (islower(Piece) ? 1 : -1))][C - 'a'] = Defender;
				if (is_piece_attacked(King, A, B, (const char (*)[8])new_board, 1) == 0)
					return 0;
			}
			break;
		}
		case 'R':
		case 'B':
		case 'Q':  // we'll try to cover our king
		{
			const signed char Distance = (abs(A - C) >= abs(B - D) ? abs(A - C) : abs(B - D));
			char x = A, to_x = (C - A) / Distance;
			signed char y = B, to_y = (D - B) / Distance;
			if (Distance > 1)
				for (x += to_x, y += to_y; !(x == C && y == D); x += to_x, y += to_y)
					for (signed char i = -1; is_piece_attacked(Piece, x, y, Board, i); --i) {  // can we COVER the king?
						if (toupper(store_piece.type) == 'K')  // king can't cover himself
							continue;
						new_board[8 - store_piece.numerical][store_piece.alphabetical - 'a'] = ' ';
						new_board[8 - D][C - 'a'] = store_piece.type;
						if (is_piece_attacked(King, A, B, (const char (*)[8])new_board, 1) == 0)
							return 0;
						new_board[8 - D][C - 'a'] = Piece;
						new_board[8 - store_piece.numerical][store_piece.alphabetical - 'a'] = store_piece.type;
					}
			break;
		}
		default:  // enemy KNIGHT or KING (we can't cover our king)
			return 1;
	}
	return 1;
}

// Determines if king cannot move.
bool does_king_cannot_move(const struct structure_piece *King, char (*board)[8])
{
	const char Base = King->type - ('K' - 'A');
	const char A = King->alphabetical;
	const signed char B = King->numerical;
	board[8 - B][A - 'a'] = ' ';  // king is temporarily removed
	for (signed char i = 0, x, y; i < 8; ++i) {
		x = A + instructions_king[i].to_x;
		y = B + instructions_king[i].to_y;
		if (are_coords_valid(x, y) == 0)  // we're out of the board
			continue;
		if (board[8 - y][x - 'a'] >= Base && board[8 - y][x - 'a'] <= Base + ('Z' - 'A'))  // king can't capture it's own piece
			continue;
		if (is_piece_attacked(King->type, x, y, (const char (*)[8])board, 1))  // this field is attacked
			continue;
		board[8 - B][A - 'a'] = King->type;  // king returns to it's field
		return 0;  // king can escape
	}
	board[8 - B][A - 'a'] = King->type;  // king returns to it's field
	return 1;  // king can't escape
}

/* This function exists for disambiguation purposes.
 * Piece 'piece' from coords (previous_x, previous_y) didn't move. Instead we'll
 * move the same type of piece from coords (new_x, new_y). The question is:
 * Does after this move our king will be save? */
bool is_king_defended_after_move(char piece, char new_x, signed char new_y, char previous_x, signed char previous_y, const char (*Board)[8])
{
	char king = (piece >= 'A' && piece <= 'Z' ? 'K' : 'k');  // white king can't be under attack after white's move
	char (*new_board)[8] = (char (*)[8])malloc(8 * 8 * sizeof(char));
	memcpy(new_board, Board, 8 * 8 * sizeof(char));
	new_board[8 - previous_y][previous_x - 'a'] = piece;
	new_board[8 - new_y][new_x - 'a'] = ' ';
	struct structure_piece king_placement;
	if (find_piece(king, &king_placement, (const char (*)[8])new_board))
		if (is_piece_attacked(king, king_placement.alphabetical, king_placement.numerical, (const char (*)[8])new_board, 1) == 0) {
			free(new_board);
			return 1;
		}
	free(new_board);
	return 0;
}

// Searches horizontally, vertically or diagonally for a competing piece from the field we just moved into.
void increment_and_check(char piece, char x, signed char y, char previous_x, signed char previous_y, const char (*Board)[8], bool *is_other, bool *in_line, bool *in_column, char to_x, signed char to_y)
{
	for (x += to_x, y += to_y; are_coords_valid(x, y) && !(x == previous_x && y == previous_y); x += to_x, y += to_y)
		if (Board[8 - y][x - 'a'] == piece)  // we found the same type of piece in the same color
			if (is_king_defended_after_move(piece, x, y, previous_x, previous_y, Board)) {  // we must disambiguate our last move
				if (x == previous_x)
					*in_column = 1;
				if (y == previous_y)
					*in_line = 1;
				*is_other = 1;
			}
	return;
}

// Determines if king is under attack while castling.
bool is_king_checked_while_castling(char king, char x, signed char y, const char (*Board_1)[8], const char (*Board_2)[8], signed char to_x)
{
	if (is_piece_attacked(king, x, y, Board_1, 1) || is_piece_attacked(king, x + to_x, y, Board_2, 1) || is_piece_attacked(king, x + 2 * to_x, y, Board_2, 1))
		return 1;
	return 0;
}

// Determines if a queen can be moved from (x1, y1) to (x2, y2).
bool is_path_straight_and_clear(char x1, signed char y1, char x2, signed char y2, const char (*Board)[8])
{
	if (x1 != x2 && y1 != y2 && abs(x1 - x2) != abs(y1 - y2))
		return 0;
	signed char distance = (abs(x1 - x2) >= abs(y1 - y2) ? abs(x1 - x2) : abs(y1 - y2));
	char x = x1, to_x = (x2 - x1) / distance;
	signed char y = y1, to_y = (y2 - y1) / distance;
	for (x += to_x, y += to_y; x != x2 && y != y2; x += to_x, y += to_y)
		if (Board[8 - y][x - 'a'] != ' ')
			return 0;
	return 1;
}

const char *Ordinal_Number_Suffix(int number)
{
	if (number % 100 >= 11 && number % 100 <= 13)
		return Suffixes.th;
	if (number % 10 == 1)
		return Suffixes.st;
	if (number % 10 == 2)
		return Suffixes.nd;
	if (number % 10 == 3)
		return Suffixes.rd;
	return Suffixes.th;
}

// Removes specified castling availability.
void remove_castling(char type, char castling_prospects[])
{
	if (strchr(castling_prospects, type) != NULL) {
		*strchr(castling_prospects, type) = '-';
		if (strlen(castling_prospects) > 1)
			for (char *pointer = strchr(castling_prospects, '-'); *pointer != '\0'; ++pointer)
				*pointer = *(pointer + 1);
	}
	return;
}

// USAGE: swap_pointers((void **)&pointer_1, (void **)&pointer_2);
static inline void swap_pointers(void **pointer_1, void **pointer_2)
{
	void *store = *pointer_1;
	*pointer_1 = *pointer_2;
	*pointer_2 = store;
	return;
}

/* Function determines one of the following types of move:
 * W_None - standard move
 * W_In_Line - we must disambiguate moving piece in line
 * W_In_Column - we must disambiguate moving piece in column
 * W_Both - we must disambiguate moving piece BOTH in column AND in line
 * W_Other - we must disambiguate moving piece EITHER in column OR in line
 * W_Pawn_None - standard pawn move
 * W_Pawn_Capture - pawn capture
 * W_Pawn_Promotion - pawn promotion
 * W_Pawn_Capture_Promotion - pawn capture with promotion
 * W_Discard_Fen - invalid move */
int what_to_write(const char (*Board)[8], const struct structure_field *Field, const struct structure_field *Previous_Field, char en_passant_field[], bool validate, char whose_move, char castling_prospects[])
{
	bool is_other = 0, in_line = 0, in_column = 0;
	char piece = Field->piece_after;
	const char A = Field->alphabetical, Previous_A = Previous_Field->alphabetical;
	const signed char B = Field->numerical, Previous_B = Previous_Field->numerical;
	if (validate == 1 && (whose_move == 'w' && islower(piece) || whose_move == 'b' && isupper(piece)))  // we moved the opponent's piece
		return W_Discard_Fen;
	if (validate == 1 && (isupper(piece) && isupper(Field->piece_before) || islower(piece) && islower(Field->piece_before)))  // we captured our own piece
		return W_Discard_Fen;
	if (Previous_Field->piece_before != Field->piece_after)  // pawn promotion
		piece = '@';  // from this time '@' stands for a pawn promotion
	switch (toupper(piece)) {
		case '@':
			if (validate == 1 && (abs(A - Previous_A) > 1 || abs(B - Previous_B) != 1))
				return W_Discard_Fen;
			strcpy(en_passant_field, "-");
			return (Field->piece_before != ' ' ? W_Pawn_Capture_Promotion : W_Pawn_Promotion);
		case 'P':
			if (validate == 1) {
				if (abs(A - Previous_A) == 1 && (abs(B - Previous_B) != 1 || !isalpha(Previous_Field->piece_before)))
					return W_Discard_Fen;
				if (abs(B - Previous_B) == 2) {
					if (A - Previous_A != 0 || !(isupper(piece) && B == 4 && Board[8 - 3][A - 'a'] == ' ' || islower(piece) && B == 5 && Board[8 - 6][A - 'a'] == ' '))
						return W_Discard_Fen;
				} else if (abs(B - Previous_B) != 1)
					return W_Discard_Fen;
			}
			if (abs(B - Previous_B) == 2)
				sprintf(en_passant_field, "%c%hhd", A, (B + Previous_B) / 2);
			return (Field->piece_before != ' ' ? W_Pawn_Capture : W_Pawn_None);
		case 'R':
			if (validate == 1 && (is_path_straight_and_clear(Previous_A, Previous_B, A, B, Board) != 1 || A != Previous_A && B != Previous_B))
				return W_Discard_Fen;
			for (signed char i = 0; i < 4; ++i)
				increment_and_check(piece, A, B, Previous_A, Previous_B, Board, &is_other, &in_line, &in_column, instructions_rook[i].to_x, instructions_rook[i].to_y);
			break;
		case 'N':
			if (validate == 1 && !(abs(A - Previous_A) == 2 && abs(B - Previous_B) == 1 || abs(A - Previous_A) == 1 && abs(B - Previous_B) == 2))
				return W_Discard_Fen;
			for (signed char x = A, y = B, i = 0; i < 8; ++i) {
				x = A + instructions_knight[i].to_x;
				y = B + instructions_knight[i].to_y;
				if (are_coords_valid(x, y) == 0 || x == Previous_A && y == Previous_B)
					continue;
				if (Board[8 - y][x - 'a'] == piece)  // an competing knight is found
					if (is_king_defended_after_move(piece, x, y, Previous_A, Previous_B, Board)) {  // we must disambiguate our last move
						if (x == Previous_A)
							in_column = 1;
						if (y == Previous_B)
							in_line = 1;
						is_other = 1;
					}
			}
			break;
		case 'B':
			if (validate == 1 && (is_path_straight_and_clear(Previous_A, Previous_B, A, B, Board) != 1 || A == Previous_A || B == Previous_B))
				return W_Discard_Fen;
			for (signed char i = 0; i < 4; ++i)
				increment_and_check(piece, A, B, Previous_A, Previous_B, Board, &is_other, &in_line, &in_column, instructions_bishop[i].to_x, instructions_bishop[i].to_y);
			break;
		case 'Q':
			if (validate == 1 && is_path_straight_and_clear(Previous_A, Previous_B, A, B, Board) != 1)
				return W_Discard_Fen;
			for (signed char i = 0; i < 4; ++i)
				increment_and_check(piece, A, B, Previous_A, Previous_B, Board, &is_other, &in_line, &in_column, instructions_rook[i].to_x, instructions_rook[i].to_y);
			for (signed char i = 0; i < 4; ++i)
				increment_and_check(piece, A, B, Previous_A, Previous_B, Board, &is_other, &in_line, &in_column, instructions_bishop[i].to_x, instructions_bishop[i].to_y);
			break;
		case 'K':
			if (validate == 1) {
				if (abs(A - Previous_A) > 1 || abs(B - Previous_B) > 1)
					return W_Discard_Fen;
				remove_castling(piece == 'k' ? 'q' : 'Q' , castling_prospects);
				remove_castling(piece == 'k' ? 'k' : 'K' , castling_prospects);
			}
			break;
	}
	strcpy(en_passant_field, "-");
	if (in_column == 1 && in_line == 1)
		return W_Both;
	if (in_column == 1 && in_line == 0)
		return W_In_Column;
	if (in_column == 0 && in_line == 1)
		return W_In_Line;
	if (is_other == 1)
		return W_Other;
	return W_None;
}

int main(int argc, char *argv[])
{
	const char Help[] = "fens2pgn - converts multiple FENs into single PGN file\n"
	"Syntax: fens2pgn [arguments] [output file] [input file]\n"
	"Arguments:\n"
	"  -f    force validity of every chess move\n"
	"  -o    take next argument as output file\n"
	"  -q    quiet - doesn't print informations to stderr\n"
	"  -v    verbose - notify of every skipped FEN\n"
	"  -h, --help    print this help text\n"
	"  -u, --usage   short usage information\n"
	"  -V, --version display program version";
	if (argc < 2) {
		puts(Help);
		return 0;
	}
	struct structure_parameters {
		bool validate;
		bool quiet;
		bool verbose;
		char *read_from_file;
		char *write_to_file;
	} parameters = {0, 0, 0, NULL, NULL};
	const struct option long_options[] = {
		{"help", 0, NULL, 'h'},
		{"usage", 0, NULL, 'u'},
		{"version", 0, NULL, 'V'},
		{NULL, 0, NULL, 0}
	};
	for (int option, long_option_index; (option = getopt_long(argc, argv, "fho:quvV", long_options, &long_option_index)) != -1;)
		switch (option) {
			case 'f':
				parameters.validate = 1;
				break;
			case 'h':
				puts(Help);
				return 0;
			case 'o':
				parameters.write_to_file = optarg;
				break;
			case 'q':
				parameters.quiet = 1;
				parameters.verbose = 0;
				break;
			case 'u':
				puts("Syntax: fens2pgn [-fqv] [-o OUTPUT_FILE] [INPUT_FILE]\n"
				"                 [-h|--help] [-u|--usage] [-V|--version]");
				return 0;
			case 'v':
				parameters.quiet = 0;
				parameters.verbose = 1;
				break;
			case 'V':
				puts("fens2pgn " VERSION "\n"
				"Copyright (C) 2015 Paweł Zacharek");
				return 0;
			default:
				return EINVAL;
		}
	if (optind == argc - 1)  // 1 unknown parameter (input file name)
		parameters.read_from_file = argv[optind];
	else if (optind != argc) {  // more unknown parameters
		fputs("Invalid argument(s) found.\n", stderr);
		return EINVAL;
	}
	FILE *input = (parameters.read_from_file == NULL ? stdin : fopen(parameters.read_from_file, "r"));
	FILE *output = (parameters.write_to_file == NULL ? stdout : fopen(parameters.write_to_file, "w"));
	if (input == NULL) {
		fprintf(stderr, "Can't open file \"%.255s\" for reading.\n", parameters.read_from_file);
		return ENOENT;
	}
	if (output == NULL) {
		fprintf(stderr, "Can't open file \"%.255s\" for writing.\n", parameters.read_from_file);
		return EACCES;
	}
	char fen_buffer[MAX_FEN_LENGHT + 1], fen_placement_buffer[MAX_PLACEMENT_LENGHT + 1];
	char store_space[STORE_SPACE_SIZE + 1], store_move[STORE_MOVE_SIZE + 1];
	char board_buffer_1[8][8], board_buffer_2[8][8];
	char (*board_1)[8] = board_buffer_1, (*board_2)[8] = board_buffer_2;
	int fen_number = 0, move_number = 1, number_of_characters_in_line = 0;
	struct structure_field distinctions[4], *field, *previous_field;
	struct structure_piece king_placement;
	signed char number_of_differences;
	char control_character, whose_move = 'w', castling_prospects[4 + 1] = "KQkq", en_passant_field[2 + 1] = "-";
	char control_string[8 + 1], result[7 + 1];  // to store respectively "Result \"" and "1/2-1/2"
	result[0] = '\0';
	bool metadata_included = 0, fens_are_incomplete = 0, first_move_number_already_written = 0, move_is_invalid = 0;
	// beginning of METADATA section
	fscanf(input, " %c", &control_character);
	while (control_character == '[') {
		metadata_included = 1;
		putc('[', output);
		if (fscanf(input, "%8[^\n]", control_string) == 1) {
			if (strncmp(control_string, "Result \"", 8) == 0 && fscanf(input, "%7[-01/2*]", result) == 1)
				fprintf(output, "%s%s", control_string, result);
			else
				fputs(control_string, output);
		}
		for (int character; (character = getc(input)) != EOF;) {
			putc(character, output);
			if (character == '\n')
				break;
		}
		fscanf(input, " %c", &control_character);
	}
	ungetc(control_character, input);
	// beginning of GAME section
	if (fscanf(input, "%" STR(MAX_FEN_LENGHT) "[-0-9a-h/rnqkpRNBQKP w]", fen_buffer) != 1) {  // reading the first FEN
		fputs("No valid FEN found.\n", stderr);
		return EILSEQ;
	}
	++fen_number;
	sscanf(fen_buffer, "%" STR(MAX_PLACEMENT_LENGHT) "[1-8/rnbqkpRNBQKP]", fen_placement_buffer);
	if (strncmp(fen_buffer, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 56) != 0) {  // it isn't classical complete FEN
		if (sscanf(fen_buffer, "%*[1-8/rnbqkpRNBQKP] %c %4s %2s %*d %d", &whose_move, castling_prospects, en_passant_field, &move_number) != 4) {  // FENs are incomplete
			fens_are_incomplete = 1;
			if (parameters.quiet != 1)
				fputs("FENs considered incomplete.\n", stderr);  // not corrupts redirected output
			whose_move = 'w';
			strcpy(castling_prospects, "KQkq");
			strcpy(en_passant_field, "-");
			move_number = 1;
		}
		if (fens_are_incomplete == 0 || strncmp(fen_buffer, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR", 43) != 0)  // starting position isn't classical
			fprintf(output, "[SetUp \"1\"]\n[FEN \"%s\"]\n", fen_buffer);
	}
	if (metadata_included == 1)
		putc('\n', output);
	if (fen2board(fen_placement_buffer, board_1, parameters.validate) == 0) {
		fputs("There are more than 2 kings or invalid pieces on the first board. Terminating.\n", stderr);
		return 0;
	}
	for (;;) {
		if (fscanf(input, " %" STR(MAX_FEN_LENGHT) "[-0-9a-h/rnqkpRNBQKP w]", fen_buffer) != 1)
			break;
		++fen_number;
		sscanf(fen_buffer, "%" STR(MAX_PLACEMENT_LENGHT) "[1-8/rnbqkpRNBQKP]", fen_placement_buffer);
		if (fen2board(fen_placement_buffer, board_2, parameters.validate) == 0) {
			fprintf(stderr, "There are more than 2 kings or invalid pieces on board nr %d. Terminating.\n", fen_number);
			return 0;
		}
		number_of_differences = compare_boards((const char (*)[8])board_1, (const char (*)[8])board_2, distinctions);
		if (number_of_differences < 2) {
			if (parameters.verbose == 1)
				fprintf(stderr, "Skipped \"%s\" (%d%s FEN).\n", fen_buffer, fen_number, Ordinal_Number_Suffix(fen_number));
			continue;
		}
		if (number_of_differences > 4) {
			fprintf(stderr, "Can't compare \"%s\" (%d%s FEN) with the previous one.\n", fen_buffer, fen_number, Ordinal_Number_Suffix(fen_number));
			return EILSEQ;
		}
		switch (number_of_differences) {
			case 2:  // ordinary move or capture
				if (distinctions[0].piece_after != ' ') {
					field = &distinctions[0];
					previous_field = &distinctions[1];
				} else {  // it means that (distinctions[1].piece_after != ' ')
					field = &distinctions[1];
					previous_field = &distinctions[0];
				}
				switch (what_to_write((const char (*)[8])board_2, (const struct structure_field *)field, (const struct structure_field *)previous_field, en_passant_field, parameters.validate, whose_move, castling_prospects)) {
					case W_Pawn_None:
						snprintf(store_move, STORE_MOVE_SIZE + 1, "%c%hhd", field->alphabetical, field->numerical);
						break;
					case W_Pawn_Capture:
						snprintf(store_move, STORE_MOVE_SIZE + 1, "%cx%c%hhd", previous_field->alphabetical, field->alphabetical, field->numerical);
						break;
					case W_Pawn_Promotion:
						snprintf(store_move, STORE_MOVE_SIZE + 1, "%c%hhd=%c", field->alphabetical, field->numerical, toupper(field->piece_after));
						break;
					case W_Pawn_Capture_Promotion:
						snprintf(store_move, STORE_MOVE_SIZE + 1, "%cx%c%hhd=%c", previous_field->alphabetical, field->alphabetical, field->numerical, toupper(field->piece_after));
						break;
					case W_None:
						snprintf(store_move, STORE_MOVE_SIZE + 1, field->piece_before != ' ' ? "%cx%c%hhd" : "%c%c%hhd", toupper(field->piece_after), field->alphabetical, field->numerical);
						break;
					case W_In_Line:
					case W_Other:
						snprintf(store_move, STORE_MOVE_SIZE + 1, field->piece_before != ' ' ? "%c%cx%c%hhd" : "%c%c%c%hhd", toupper(field->piece_after), previous_field->alphabetical, field->alphabetical, field->numerical);
						break;
					case W_In_Column:
						snprintf(store_move, STORE_MOVE_SIZE + 1, field->piece_before != ' ' ? "%c%hhdx%c%hhd" : "%c%hhd%c%hhd", toupper(field->piece_after), previous_field->numerical, field->alphabetical, field->numerical);
						break;
					case W_Both:
						snprintf(store_move, STORE_MOVE_SIZE + 1, field->piece_before != ' ' ? "%c%c%hhdx%c%hhd" : "%c%c%hhd%c%hhd", toupper(field->piece_after), previous_field->alphabetical, previous_field->numerical, field->alphabetical, field->numerical);
						break;
					case W_Discard_Fen:
						move_is_invalid = 1;
						break;
					}
				break;
			case 3:  // en passant capture
				if (parameters.validate != 1) {
					if (whose_move == 'w')
						snprintf(store_move, STORE_MOVE_SIZE + 1, "%cx%c%hhd", distinctions[0].alphabetical == distinctions[2].alphabetical ? distinctions[1].alphabetical : distinctions[2].alphabetical, distinctions[0].alphabetical, distinctions[0].numerical);
					else
						snprintf(store_move, STORE_MOVE_SIZE + 1, "%cx%c%hhd", distinctions[0].alphabetical == distinctions[2].alphabetical ? distinctions[1].alphabetical : distinctions[0].alphabetical, distinctions[2].alphabetical, distinctions[2].numerical);
				} else {
					char all_fields[6 + 1];
					snprintf(all_fields, sizeof all_fields, "%c%c%c%c%c%c", distinctions[0].piece_before, distinctions[0].piece_after, distinctions[1].piece_before, distinctions[1].piece_after, distinctions[2].piece_before, distinctions[2].piece_after);
					if (whose_move == 'w' && en_passant_field[0] == distinctions[0].alphabetical && en_passant_field[1] - '0' == distinctions[0].numerical && distinctions[0].numerical == distinctions[2].numerical + 1 && distinctions[1].alphabetical == distinctions[2].alphabetical - 1 && (
						distinctions[0].alphabetical == distinctions[2].alphabetical && memcmp(all_fields, " PP p ", 6) == 0
						|| distinctions[0].alphabetical != distinctions[2].alphabetical && memcmp(all_fields, " Pp P ", 6) == 0
					))
						snprintf(store_move, STORE_MOVE_SIZE + 1, "%cx%c%hhd", distinctions[0].alphabetical == distinctions[2].alphabetical ? distinctions[1].alphabetical : distinctions[2].alphabetical, distinctions[0].alphabetical, distinctions[0].numerical);
					else if (whose_move == 'b' && en_passant_field[0] == distinctions[2].alphabetical && en_passant_field[1] - '0' == distinctions[2].numerical && distinctions[0].numerical == distinctions[2].numerical + 1 && distinctions[0].alphabetical == distinctions[1].alphabetical - 1 && (
						distinctions[0].alphabetical == distinctions[2].alphabetical && memcmp(all_fields, "P p  p", 6) == 0
						|| distinctions[0].alphabetical != distinctions[2].alphabetical && memcmp(all_fields, "p P  p", 6) == 0
					))
						snprintf(store_move, STORE_MOVE_SIZE + 1, "%cx%c%hhd", distinctions[0].alphabetical == distinctions[2].alphabetical ? distinctions[1].alphabetical : distinctions[0].alphabetical, distinctions[2].alphabetical, distinctions[2].numerical);
					else
						move_is_invalid = 1;
				}
				break;
			case 4:  // castling
				if (parameters.validate != 1)
					snprintf(store_move, STORE_MOVE_SIZE + 1, distinctions[0].alphabetical == 'a' ? "O-O-O" : "O-O");
				else {
					if (distinctions[0].alphabetical == 'a' && is_king_checked_while_castling(distinctions[3].piece_before, distinctions[3].alphabetical, distinctions[3].numerical, (const char (*)[8])board_1, (const char (*)[8])board_2, -1) == 0 && (
						whose_move == 'w' && strchr(castling_prospects, 'Q') != NULL && memcmp(&board_1[8 - 1][0], "R   K", 5) == 0 && memcmp(&board_2[8 - 1][0], "  KR ", 5) == 0
						|| whose_move == 'b' && strchr(castling_prospects, 'q') != NULL && memcmp(&board_1[8 - 8][0], "r   k", 5) == 0 && memcmp(&board_2[8 - 8][0], "  kr ", 5) == 0
					))
						snprintf(store_move, STORE_MOVE_SIZE + 1, "O-O-O");
					else if (distinctions[3].alphabetical == 'h' && is_king_checked_while_castling(distinctions[0].piece_before, distinctions[0].alphabetical, distinctions[0].numerical, (const char (*)[8])board_1, (const char (*)[8])board_2, 1) == 0 && (
						whose_move == 'w' && strchr(castling_prospects, 'K') != NULL && memcmp(&board_1[8 - 1][4], "K  R", 4) == 0 && memcmp(&board_2[8 - 1][0], " RK ", 4) == 0
						|| whose_move == 'b' && strchr(castling_prospects, 'k') != NULL && memcmp(&board_1[8 - 8][4], "k  r", 4) == 0 && memcmp(&board_2[8 - 8][0], " rk ", 4) == 0
					))
						snprintf(store_move, STORE_MOVE_SIZE + 1, "O-O");
					else
						move_is_invalid = 1;
				}
				break;
		}
		if (parameters.validate == 1 && move_is_invalid == 1) {
			move_is_invalid = 0;
			if (parameters.quiet != 1)
				fprintf(stderr, "Skipped \"%s\" (%d%s FEN) because the calculated move is invalid.\n", fen_buffer, fen_number, Ordinal_Number_Suffix(fen_number));
			continue;
		}
		if (parameters.validate == 1 && find_piece(whose_move == 'w' ? 'K' : 'k', &king_placement, (const char (*)[8])board_2))
			if (is_piece_attacked(king_placement.type, king_placement.alphabetical, king_placement.numerical, (const char (*)[8])board_2, 1)) {
				if (parameters.quiet != 1)
					fprintf(stderr, "Skipped \"%s\" (%d%s FEN) because we're still in check.\n", fen_buffer, fen_number, Ordinal_Number_Suffix(fen_number));
				continue;
			}
		if (find_piece(whose_move == 'w' ? 'k' : 'K', &king_placement, (const char (*)[8])board_2))
			if (is_piece_attacked(king_placement.type, king_placement.alphabetical, king_placement.numerical, (const char (*)[8])board_2, 1)) {  // king is checked
				if (does_king_cannot_move((const struct structure_piece *)&king_placement, board_2) && (
					is_piece_attacked(king_placement.type, king_placement.alphabetical, king_placement.numerical, (const char (*)[8])board_2, 2)  // double check
					|| is_it_checkmate(king_placement.alphabetical, king_placement.numerical, (const char (*)[8])board_2, (const char *)en_passant_field)
				))
					snprintf(store_move + strlen(store_move), STORE_MOVE_SIZE + 1 - strlen(store_move), "#");
				else
					snprintf(store_move + strlen(store_move), STORE_MOVE_SIZE + 1 - strlen(store_move), "+");
			}
		if (first_move_number_already_written == 1)
			strcpy(store_space, " ");
		if (whose_move == 'w' || first_move_number_already_written == 0)
			snprintf(store_space + (first_move_number_already_written ? 1 : 0), STORE_SPACE_SIZE + 1 - (first_move_number_already_written ? 1 : 0), "%d. ", move_number);
		number_of_characters_in_line += strlen(store_space) - 1;  // we can omit trailing space
		if (number_of_characters_in_line >= 80) {
			store_space[0] = '\n';
			number_of_characters_in_line = strlen(store_space) - 2;  // the first space is also not included
		}
		number_of_characters_in_line += strlen(store_move) + 1;  // add previously disregarded trailing space
		if (number_of_characters_in_line >= 80) {
			store_space[strlen(store_space) - 1] = '\n';
			number_of_characters_in_line = strlen(store_move);  // doesn't count trailing space of 'store_space' array
		}
		fprintf(output, "%s%s", store_space, store_move);
		first_move_number_already_written = 1;
		if (whose_move == 'b')
			++move_number;
		whose_move = (whose_move == 'b' ? 'w' : 'b');
		swap_pointers((void **)&board_1, (void **)&board_2);
	}
	if (result[0] != '\0')
		fprintf(output, "%c%s\n", number_of_characters_in_line + 1 + strlen(result) >= 80 ? '\n' : ' ', result);
	else
		fprintf(output, "%c*\n", number_of_characters_in_line + 2 >= 80 ? '\n' : ' ');
	if (parameters.read_from_file != NULL)
		fclose(input);
	if (parameters.write_to_file != NULL)
		fclose(output);
	return 0;
}
