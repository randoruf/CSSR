////////////////////////////////////////////////////////////////////////
//Title:	G_Array.h
//Author:	Kristina Klinkner
//Date:		March 20, 2002
//Description:	Header file for G_Array.cpp
//
////////////////////////////////////////////////////////////////////////
#ifndef __G_ARRAY_H
#define __G_ARRAY_H

#define G_INITIAL_SIZE 50
#define G_INCREMENT 10

#include "Common.h"

class ArrayElem {
public:
    ArrayElem() {
      m_string = NULL;
      m_counts = NULL;
    }

    ~ArrayElem() {
      if (m_counts) { delete[] m_counts; }
      if (m_string) { delete[] m_string; }
    }

    void setChar(char symbol[]);

    char *getString() { return m_string; }

    std::string getHistory() {
      std::string stringifyed (m_string);
      return stringifyed;
    }

    void setString(char string[]);

    void setCounts(int counts[], int length);

    int *getCounts() { return m_counts; }

    std::string toString() {

      std::string dist = "";
      int total = 0;
      for (int i = 0; i < sizeof(m_counts)/sizeof(m_counts[0]); i++) {
        total += m_counts[i];
        dist += to_string(m_counts[i]) + "\t";
      }
      return "ArrayElem {m_string: " + getHistory() + ", m_counts:["+dist+"], total: "+std::to_string(total)+"}";
    };

protected:
    char *m_string;

    int *m_counts;
};


class G_Array {
public:
    G_Array() {
      m_size = 0;
      m_G_ArrayList = new ArrayElem *[G_INITIAL_SIZE];
      m_maxsize = G_INITIAL_SIZE;
    }

    ~G_Array();

    bool Full() { return (m_size == m_maxsize - 1); }

    void Insert(char string[], int counts[], int length);

    void Grow(int newsize);

    int getSize() { return m_size; }

    ArrayElem **getList() { return m_G_ArrayList; }

private:

    ArrayElem **m_G_ArrayList; //array of strings
    int m_size;      //size of array
    int m_maxsize;    //maximum size of array
};

#endif
