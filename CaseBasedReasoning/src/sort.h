// http://stackoverflow.com/questions/6499413/does-stl-or-boost-provide-any-clean-way-to-get-the-sort-order-without-reordering
// template<class T>
class sort_order {
public:
  sort_order(const std::vector<double> *_sortArray) : sortArray(_sortArray) {;}
  
  bool operator()(int lhs, int rhs) const {
    return sortArray[lhs] < sortArray[rhs];
  }
  
private:
  const std::vector<double> *sortArray;
};

template<typename A, typename B>
std::pair<B,A> flip_pair(const std::pair<A,B> &p)
{
  return std::pair<B,A>(p.second, p.first);
}

template<typename A, typename B>
std::multimap<B,A> flip_map(const std::map<A,B> &src)
{
  std::multimap<B,A> dst;
  std::transform(src.begin(), src.end(), std::inserter(dst, dst.begin()), 
                 flip_pair<A,B>);
  return dst;
}
